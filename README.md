# ChatRbot
A simple interface from R to the Anthropic Claude API. This implementation takes some inspiration from [jcrodriguez1989/chatgpt](https://github.com/jcrodriguez1989/chatgpt/) package to communicate with ChatGPT

## Installation
Install the development version from
[GitHub](https://github.com/edubruell/ChatRbot/) with:

``` r
# install.packages("remotes")
remotes::install_github("edubruell/ChatRbot")
```
## Requirements

You need to setup your Anthropic API key in R.

First you will need to obtain your Anthropic API key. You can create an
API key in your [Anthropic account settings
page](https://console.anthropic.com/settings/keys).

You should not have your API key directly stored in any source file of a given project.
However, if you just want to try ChatRbot you can set it with: 

``` r
Sys.setenv(ANTHROPIC_API_KEY = "XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
```

Or you can do it persistent (session-wide), by assigning it in your
`.Renviron` file. For this, execute `usethis::edit_r_environ()`, and add a
line  at the end of this file with your API key:

``` r
ANTHROPIC_API_KEY=XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```


## Features
The package comes with three main functions:
1. ``anthropic_api_call`` handles the communication with the API in the background
2. ``print_conversation`` prints out conversations in colour and with some basic formating to the console
3. ``chat``is the main chat function with a wide range of features such as:
   - Support for saved conversations to the current R environment (including autosaves) and the ability to send prompts to pre-exsisting saved covnersation (See documentation on the **.c** arguement)
   - Ability to send the output of (anonoymous) R functions directly to ChatGPT via the **.f** argument
   - Ability to send image files to the Claude API
   - Ability to capture the last plot and send it to the Claude API
4. ``mc_answer`` allows you to specify **.prompt** and a list of multiple choices **.choices**.The function allways returns one the choices as answer to your prompt

## Code Examples

Send a simple message.
```r
chat("Give me some R code that creates a random tibble with a simple test dataset with 5000 observations that has a random gender variable,
a variable for occupational status and a wage variable that is a linear function of gender, occupational status and some other observables.
Be creative and think what other wage relevant observables can be in a typical large social survey and add some of them into your example code. 
")
```

If autosave is enabled the conversation is saved in a list in the R enivronment with a name in the pattern of ``convo_as0``,``convo_as1``,``convo_as2``,etc.

You can also set the name of the object manually in the **.c** argument for a new conversation or send a past conversation from an existing object in the current environment to ChatGPT:

```r
#The .c option creates a new object in the parent environment of the call
#This object is filled with the message history. If an object named in .c 
#allready exsist the previous messages stored in .c are used in the request

chat("
Here is an example for a bibtex entry in my bibliography:
@TechReport{Gartner2005,
  author      = {Gartner, Hermann},
  title       = {The Imputation of Wages Above the Contribution Limit with the German IAB Employment Sample},
  institution = {Institut für Arbeitsmarkt- und Berufsforschung (IAB), Nürnberg [Institute for Employment Research, Nuremberg, Germany]},
  year        = {2005},
  type        = {{FDZ Methodenreport}},
}

Please make a BibTex key for the following method report following the style of my example:

series: FDZ-METHODENREPORT
Methodological aspects of labour market data
date: 01|2023 
title: AKM effects for German labour market data 1985-2021
authors: Benjamin Lochner, Stefan Seth, Stefanie Wolter",
.sys="You are an expert on LaTeX and BibTex and only reply with valid bibtex code",
.c  ="bibtex_example")


#Adding .c if the object allready exists adds a reply to a conversation. 
#Here a copy-pasted reference from a paper is sent to the previous conversation.
chat("Cahuc, Pierre, Fabien Postel-Vinay, and Jean-Marc Robin, ‘‘Wage Bargaining
with On the Job Search: Theory and Evidence,’’ Econometrica, 74, no. 2
(2006), 323–364.",.c="bibtex_example")
```

You can also change different parameters for the Claude conversation such as the systems message or the temperature of the conversation:
``` r
#What does the System message do?
#ChatGPT takes the role you give it in the system message, but sometimes deviates a little bit more from the system message
chat("Was ist der deutsche Mikrozensus?",
     .sys = "You are a poetic soul and only reply with english 4 verse poems")

#Temperature sets the randomness of the answer
chat("Was ist der deutsche Mikrozensus?")

#0 is one extreme where the output becomes fully deterministic. 
#Else the next token is allways sampled from a list of the most likely tokens. Here only the most likely token is used every time.
#Sometimes with 0 ChatGPT only exports a stop token. Then you just have to choose something higher 
#and you can not get decent deterministic answers.
chat("Was ist der deutsche Mikrozensus?",.temp=0)
chat("Was ist der deutsche Mikrozensus?",.temp=0) # Same answer
#If you need deterministic answers that you can describe well in a paper this is probably the way to go.
#For mc_answer .temp=0 is the reasonable default, while it is higher for chat.

#One is the other extreme (most random)
chat("Was ist der deutsche Mikrozensus?",.temp=1)
#my example here is not super good, but temperature can have strong effects on long prompts and coding questions. 

```

One nice feature is that you can directly send R console output to Claude. This is really useful to create some quick text descriptions:
```r

# Some example data to show how passing R outputs to Claude works. I wonder how I came up with that :-)
library(tidyverse)

example_data <- tibble(gender              = sample(c("Male", "Female"), 5000, replace = TRUE),
                       occupational_status = sample(c("Employed", "Unemployed", "Student", "Retired"), 5000, replace = TRUE),
                       age                 = rnorm(5000, mean = 35, sd = 10), 
                       education           = sample(c("High School", "Some College", "Bachelor's Degree", "Master's Degree", "Doctorate"), 5000, replace = TRUE), 
                       years_of_experience = rnorm(5000, mean = 10, sd = 5), 
                       industry_sector     = sample(c("Manufacturing", "Finance", "Healthcare", "Retail", "Education"), 5000, replace = TRUE)) %>%
  mutate(wage = 50000 
         + 10000 * (gender == "Male") 
         + 2000 * (occupational_status == "Employed") 
         + 500 * age 
         + 5000 * as.integer(factor(education, levels = c("High School", "Some College", "Bachelor's Degree", "Master's Degree", "Doctorate")))
         + 1000 * years_of_experience 
         + 3000 * (industry_sector == "Finance")
         + 1500 * rnorm(5000))

#Make a simple regression table with etable that we will also use in the example
#This code can be added embeded in a function in the .f arguement. Then we can 
#automatically add its output to the prompt.
library("fixest")
feols(wage ~ age + csw0(gender,education), example_data, vcov = 'hetero') %>%
  etable()

#Let's add a function .f which console output is also sent to ChatGPT
chat("Please give me an interpretation of the results in column 3 of the regression table below. 
      What do the coefficients of the education variable mean? 
      What is the baseline level for education the coefficients compare to?
",
     .f = ~{ #Here is how you add the output of code. All output from the function(){} is captured and added to the prompt
       print("Here are counts of all levels of the education variable including the baseline:")
       example_data %>%
         count(education) %>%
         print()
       
       print("Here is a regression table")
       feols(wage ~ age + csw0(gender,education), example_data, vcov = 'hetero') %>%
         etable()
     })
```

Note that you can also disable autosave and get a message list from Claude by 
setting **.return_history** to `TRUE`:
```r
cheese <- chat("Explain why cheese is often yellow",
               .auto_save = FALSE,
               .return_history = TRUE,
               .print = FALSE)
```

The chat function can also use image files or capture the last plot in the message it send to Claude:

```r
library(tidyverse)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Car Weight vs. Miles per Gallon",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon")

# Call the function with the ggplot and a prompt
chat(
  .prompt = "Please analyze the relationship between car weight and miles per gallon based on the provided plot.",
  .capture_plot = TRUE,
  .model = "claude-3-opus-20240229",
  .max_tokens = 100,
  .temperature = 0.7,
  .c = "car_analysis_convo"
)
```
The multiple choice function is particularly useful when you want to restrict the type of answer you get from Claude to a fixed vector of 
examples:
```r
# Define the prompt and choices
prompt <- "What is the capital of France?"
choices <- c("London", "Paris", "Berlin", "Madrid")

# Call the mc_answer function
mc_answer(prompt, choices)
```

