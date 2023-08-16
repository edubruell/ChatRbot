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
    
