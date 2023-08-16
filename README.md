# ChatRbot
A minimal implementation of a chatbot from the R Console  based on the ChatGPT API. My personal implementation of chatGPT bot taking heavy inspiration from [jcrodriguez1989/chatgpt](https://github.com/jcrodriguez1989/chatgpt/)

## Installation
Install the development version from
[GitHub](https://github.com/edubruell/ChatRbot/) with:

``` r
# install.packages("remotes")
remotes::install_github("edubruell/ChatRbot")
```
## Requirements

You need to setup your ChatGPT API key in R.

First you will need to obtain your ChatGPT API key. You can create an
API key by accessing [OpenAI API
page](https://beta.openai.com/account/api-keys) -don’t miss their
article about [Best Practices for API Key
Safety](https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety)-.

Then you have to assign your API key for usage in R, this can be done
just for the actual session, by doing:

``` r
Sys.setenv(OPENAI_API_KEY = "XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
```

Or you can do it persistent (session-wide), by assigning it in your
`.Renviron` file. For it, execute `usethis::edit_r_environ()`, and in
that file write a line at the end your API key as

``` r
OPENAI_API_KEY=XX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

API keys are handled identically to [jcrodriguez1989/chatgpt](https://github.com/jcrodriguez1989/chatgpt/). So using both this package and ChatRbot works seamlessly.

## Features
The package comes with three main functions:
1. ``api_call_chatGPT`` handles the communication with the API in the background
2. ``print_conversation`` prints out conversations in colour and with some basic formating to the console
3. ``chat``is the main chat function with a wide range of features such as:
   - Support for saved conversations to the current R environment (including autosaves) and the ability to send prompts to pre-exsisting saved covnersation (See documentation on the **.c** arguement)
   - Ability to send the output of (anonoymous) R functions directly to ChatGPT via the **.f** argument

## Code Examples

Send a simple message a print it an disable autosaving of the conversation.
```r
chat("Give me some R code that creates a random tibble with a simple test dataset with 5000 observations that has a random gender variable,
a variable for occupational status and a wage variable that is a linear function of gender, occupational status and some other observables.
Be creative and think what other wage relevant observables can be in a typical large social survey and add some of them into your example code. 
",.auto_save_c = FALSE)
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

You can also change different parameters for the ChatGPT conversation such as the systems message or the temperature of the conversation:
``` r
#What does the System message do?
#ChatGPT takes the role you give it in the system message, but sometimes deviates a little bit more from the system message
chat("Was ist der deutsche Mikrozensus?",
     .sys = "You are a poetic soul and only reply with english 4 verse poems",.no_tree = TRUE)

#Temperature (0.7 is a reasonable standard)
chat("Was ist der deutsche Mikrozensus?")

#0 is one extreme where the output becomes fully deterministic. 
#Else the next token is allways sampled from a list of the most likely tokens. Here only the most likely token is used every time.
#Sometimes with 0 ChatGPT only exports a stop token. Then you just have to choose something higher 
#and you can not get decent deterministic answers.
chat("Was ist der deutsche Mikrozensus?",.temp=0)
chat("Was ist der deutsche Mikrozensus?",.temp=0) # Same answer
#If you need deterministic answers that you can describe well in a paper this is probably the way to go.

#One is the other extreme (most random)
chat("Was ist der deutsche Mikrozensus?",.temp=1)
#my example here is not super good, but temperature can have strong effects on long prompts and coding questions. 

```

One nice feature is that you can directly send R console output to ChatGPT. This is really useful to create some quick text descriptions:
```r

# Some example data to show how passing R outputs to chatGPT works. I wonder how I came up with that :-)
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
