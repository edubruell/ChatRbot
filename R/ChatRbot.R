
#' Send an API call to chatGPT
#'
#' This function sends an API call to chatGPT and retries to send out the call if it did not work
#'
#' @param .messages An message list in a predefined format (list with role and content information for each meassage)
#' @param .openai_api_key The API Key for OpenAI. By default loads one from Sys.getenv("OPENAI_API_KEY") - Set it in your .RProfile
#' @param .model The ChatGPT  modelto be used for ChatRBot. The default is "gpt-3.5-turbo",
#' @param .max_tokens The maximum number of tokens to generate in the completion.
#' @param .temp What sampling temperature to use, between 0 and 2. Higher values make the output more random. With 0 the output is fully deterministic, but likely less good.
#' @param .top_p The alernative sampling approach to temperature - set a return probability  between 0 and 1
#' @param .presence_penalty Between -2.0 and 2.0. Positive values -> new tokens penalized if they appear in the text so far
#' @param .frequency_penalty Between -2.0 and 2.0. Positive values -> new tokens penalized by frequency in the text so far
#' @param .max_retries #Retries after failed requests
#' @param .v #Do we want detailed connection details and metadata for each request
#' @param .timeout #Seconds after a request times out (Set higher for long requests)
#' @return A message list in the same format as .messages
#' @export
api_call_chatGPT <- function(.messages,
                             .openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                             .model          = "gpt-3.5-turbo", #ChatGPT Standard model
                             .max_tokens     = 512L,            #The maximum number of tokens to generate in the completion.
                             .temp           = 0.7,             #What sampling temperature to use, between 0 and 2. Higher values make the output more random. With 0 the output is fully deterministic, but likely less good.
                             .top_p          = 1,               #The alernative sampling approach to temperature - set a return probability  between 0 and 1
                             .presence_penalty  = 0,  #Between -2.0 and 2.0. Positive values -> new tokens penalized if they appear in the text so far
                             .frequency_penalty = 0,  #Between -2.0 and 2.0. Positive values -> new tokens penalized by frequency in the text so far
                             .max_retries = 3,          #Retries after failed requests
                             .v                = FALSE, #Do we want detailed connection details and metadata for each request
                             .timeout          = 25){   #Seconds after a request times out (Set higher for long requests)

  #Set parameters for request
  params <- list(
    model             = .model,
    max_tokens        = .max_tokens,
    temperature       = .temp,
    top_p             = .top_p,
    frequency_penalty = .presence_penalty,
    presence_penalty  = .frequency_penalty
  )

  #Build requests for ChatGPT type models
  if(.model %in% c("gpt-3.5-turbo","gpt-4")) {

    #Set up variables for retrying the API request
    retries <- 0
    max_retries <- .max_retries

    #Catch connection errors
    out <- tryCatch({
      #POST the request to the API
      httr::content(httr::POST(
        "https://api.openai.com/v1/chat/completions",
        httr::add_headers("Authorization" = paste("Bearer", .openai_api_key)),
        httr::content_type_json(),
        body = jsonlite::toJSON(c(params, list(messages = .messages)), auto_unbox = TRUE),
        httr::timeout(seconds = .timeout),
        if(.v==TRUE){httr::verbose()}
      ))
    }, error = function(e) {
      print(e)
      if (retries < max_retries) {
        retries <<- retries + 1
        message("ChatGPT Request Error. Retrying request...")
        Recall()
      } else {
        stop("Maximum retries exceeded.")
      }
    })
  }
  #Return a list of API call results
  return(out)
}


#' Print a chatGPT conversation
#'
#' This function prints a conversation to the console
#'
#' @param .c The conversation to be printed to console
#' @param .m A message tree to be printed (Has precedence over .c)
#' @export

print_conversation <- function(.c=NULL,.m=NULL){
  #Accept both strings and symbols
  if(is.null(.m)){.m <- as.character(rlang::ensym(.c)) |> get()}

  #The colourisation code is downright stolen from testthat
  .fg_colours <- c(
    "black" = "0;30",
    "blue" = "0;34",
    "green" = "0;32",
    "red" = "0;31",
    "yellow" = "1;33",
    "white" = "1;37"
  )

  .bg_colours <- c(
    "black" = "40",
    "red" = "41",
    "green" = "42",
    "brown" = "43",
    "blue" = "44",
    "purple" = "45",
    "cyan" = "46"
  )

  colourise <- function(text, fg = "black", bg = NULL) {
    term <- Sys.getenv()["TERM"]
    colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")


    col_escape <- function(col) {
      paste0("\033[", col, "m")
    }

    col <- .fg_colours[tolower(fg)]
    if (!is.null(bg)) {
      col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
    }

    init <- col_escape(col)
    reset <- col_escape("0")
    paste0(init, text, reset)
  }


  purrr::map2_chr(.x = .m |> purrr::map_chr("role"),
           .y = .m |> purrr::map_chr("content"),
           .f = function(name,message){
             .col <- "white"
             if(stringr::str_detect(name,"user")){
               .col <- "green"
             }
             if(stringr::str_detect(name,"system")){
               .col <- "yellow"
             }

             glue("\n\n------------------------------------------------------------------------\n\n {line}",
                  line = colourise(glue("[{name}]: {message}"),.col)
             )


           })|>
    purrr::walk(cat)


}


#' Send an API call to chatGPT
#'
#' This function sends an API call to chatGPT and retries to send out the call if it did not work
#'
#' @param .text text for the current message to chatgpt
#' @param .f Optional place to add a function, that gives it console output directly to ChatGPT
#' @param .model The ChatGPT  modelto be used for ChatRBot. The default is "gpt-3.5-turbo",
#' @param .v Make the current request verbose
#' @param .timeout How long to wait after the request times out
#' @param .sys The system message (only relevant if no  existing message list is used). Basic setup of ChatGPTs role. Has only minor influence, but is worth to try some things here.
#' @param .c Specifiy a conversation that is used to save messages or that is conteinued if it is pre-existing
#' @param .auto_save_c Should the conversation be auto-saved (only applies if .c is not set manually)
#' @param .temp What sampling temperature to use, between 0 and 2. Higher values make the output more random. With 0 the output is fully deterministic, but likely less good.
#' @param .messages #Manually supply a message tree as input. This is an alternative to setting .c. If .c is set this arguement does not work
#' @param .no_tree #Should the function return a message tree as output
#' @return A message list in the same format as .messages if .no_tree is set to FALSE
#' @export

#A function to do the actual chatting
chat <- function(.text,                              #text for the current message
                 .f        = function(){cat("")},    #Optional place to add a function, that gives it console output directly to ChatGPT
                 .v        = FALSE,                  #Make the current request verbose
                 .timeout  = 45,                     #How long to wait after the request times out
                 .sys      = "You are a helpfull assistant, labour economist and proficient R programmer.", #The system message (only relevant if no message list is used). Basic setup of ChatGPTs role. Has only minor influence, but is worth to try some things here.
                 .messages = NULL,           #Past messages
                 .no_tree  = TRUE,           #Only output current message to console. Do not return the message list or tree as an R objec
                 .temp     = 0.7,            #Temperature
                 .c        = NULL,            #An alternative way to preserve conversion states comapred to message lists
                 .auto_save_c = TRUE
){


  #Build our prompt from the .text and the console output of .f
  pr <- glue::glue("{.text}

                  {desc}",
             desc = capture.output(rlang::as_function(.f)(),file=NULL) |>
               stringr::str_c(collapse="\n"))

  #An autosave feature for conversations
  if(.auto_save_c==TRUE & is.null(.c)){
    current_convos <- ls(envir =  parent.frame()) |>
                                purrr::keep((\(x) str_detect(x,"convo_as")))

    if(length(current_convos)==0){c_number <- 0}
    if(length(current_convos)>0){
      c_number <- current_convos |>
         stringr::str_extract("\\d+") |>
         as.numeric() |>
         max() + 1
    }

    .c <- glue::glue("convo_as{c_number}")
  }

  #Allow to acces autosaves via integers
  if(is.numeric(.c)){
    .c <- glue::glue("convo_as{.c}")
  }

  #Message tree via conversation
  if(is.character(.c)){
    .no_tree= TRUE
    if(!exists(.c,inherits = TRUE, envir = parent.frame())){
      assign(x = .c,value =
               list(
                 list(role = "system", content = .sys)),
             inherits = TRUE,
             envir =  parent.frame()
      )
    }

    .messages <- get(.c)
  }


  #continue the old message tree if it is passed
  if(!is.null(.messages)){
    .messages <- .messages |>
      append(list(list(role="user", content=pr)))
  }

  #Create a new message tree if no messages were passed or no conversation is used
  if(is.null(.messages)){
    .messages <- list(
      list(role = "system", content = .sys),
      list(role = "user",   content = pr)
    )
  }

  #Call the API. Get a chatGPT object (list of request answerrs) from the API
  cgpt_obj <- api_call_chatGPT(.messages,
                               .timeout = .timeout,
                               .v = .v,
                               .temp = .temp)




  #Append the messages with the current answer
  out <- .messages |> append(list(list(role="assistant",content = cgpt_obj$choices[[1]]$message$content)))

  #Output current read
  print_conversation(.m=out[seq(length(out)-1L,length(out))])

  if(is.character(.c)){
    assign(x = .c,value =
             .messages |> append(list(list(role="assistant",content = cgpt_obj$choices[[1]]$message$content))),
           inherits = TRUE,
           envir =  parent.frame()
    )
  }

  #Do not output a message tree if no_tree is set to TRUE
  if(.no_tree==FALSE){return(out)}
}






