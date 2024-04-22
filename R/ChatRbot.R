

#' Call the Anthropic API to interact with AI models
#'
#' @param .messages A list of messages to send to the model.
#' @param .model The model identifier (default: "claude-3-opus-20240229").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return The response content as text.
#' @export
#' @examples
#' messages <- list(
#'   list(role = "user", content = "Hello Claude, how are you doing today?")
#' )
#' response <- anthropic_api_call(.messages = messages)
anthropic_api_call <- function(.messages,
                               .model = "claude-3-opus-20240229",
                               .max_tokens = 1024,
                               .temperature = NULL,
                               .top_k = NULL,
                               .top_p = NULL,
                               .system = NULL,
                               .metadata = NULL,
                               .stop_sequences = NULL,
                               .tools = NULL,
                               .api_url = "https://api.anthropic.com/v1/messages",
                               .timeout = 60) {  # Default timeout set to 60 seconds
  
  # Retrieve your API key from environment variables
  api_key <- base::Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("API key is not set.")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `x-api-key` = api_key,
    `anthropic-version` = "2023-06-01",
    `content-type` = "application/json; charset=utf-8"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = .messages,
    system = .system,
    temperature = .temperature,
    top_k = .top_k,
    top_p = .top_p,
    metadata = .metadata,
    stop_sequences = .stop_sequences,
    stream = FALSE,
    tools = .tools
  )
  
  # Filter out NULL values from the body
  body <- base::Filter(base::Negate(base::is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Check for errors
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response)$reason)
  }
  
  # Return the content of the response
  httr::content(response, "text", encoding = "UTF-8")
}



#' Print a Claude conversation
#'
#' This function prints a conversation to the console
#'
#' @param .c The conversation to be printed to console
#' @param .m A message list to be printed (Has precedence over .c)
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
  
  int_content <- .m |> purrr::map("content") |> purrr::flatten()

  purrr::map2_chr(.x = .m |> purrr::map_chr("role"),
                  .y = map(int_content,"text") |>
                    purrr::discard(is.null) |>
                    map_chr(1),
                  .f = function(name,message){
                    .col <- "white"
                    if(stringr::str_detect(name,"user")){
                      .col <- "green"
                    }
                    if(stringr::str_detect(name,"system")){
                      .col <- "yellow"
                    }
                    
                    glue::glue("\n\n------------------------------------------------------------------------\n\n {line}",
                               line = colourise(glue::glue("[{name}]: {message}"),.col)
                    )
                    
                    
                  })|>
    purrr::walk(cat)
}

#' Chat with one of the Claude models
#'
#' @param .prompt The users current
#' @param .imagefile An image to include into the user prompt
#' @param .capture_plot Should the last plot be captured and sent to Claude? (Default: FALSE)
#' @param .f Optional place to add a function, that gives it console output directly to Claude
#' @param .model The claude model to be used for ChatRBot. The default is "claude-3-opus-20240229",
#' @param .v Make the current request verbose
#' @param .timeout How long to wait after the request times out
#' @param .sys The system message. Basic setup of Claude's role. 
#' @param .c Specify a conversation that is used to save messages or that is continued if it is pre-existing
#' @param .print Should the conversation be printed (Default: TRUE)
#' @param .auto_save_c Should the conversation be auto-saved (only applies if .c is not set manually)
#' @param .temp What sampling temperature to use, between 0 and 2. Higher values make the output more random. With 0 the output is fully deterministic, but likely less good.
#' @export
chat <- function(.prompt,
                 .imagefile      = NULL,                # Optional image file path
                 .capture_plot   = FALSE,               #Capture a plot to send to Claude models
                 .f              = function(){cat("")}, #Optional place to add a function, that gives it console output directly to ChatGPT
                 .model          = NULL,                #Which model should be used
                 .max_tokens     = 1024,                #What are the max_tokens allowed
                 .v              = FALSE,               #Make the current request verbose
                 .timeout        = 60,                  #How long to wait after the request times out
                 .system         = NULL,                #The system message (only relevant if no message list is used). Basic setup of ChatGPTs role. Has only minor influence, but is worth to try some things here.
                 .temperature    = 0.7,                 #Temperature
                 .c              = NULL,                #Specify to which object conversation state is preserved
                 .print          = TRUE,                #Print conversations
                 .auto_save      = TRUE,                #Automatically return convos
                 .return_history = FALSE) {
  
  # Validate input prompt
  if (!is.character(.prompt) || length(.prompt) == 0) {
    stop("Prompt must be a non-empty string.")
  }
  
  #Build our prompt from the .prompt and the console output of .f
  .prompt <- glue::glue("{.prompt}

                  {desc}",
                   desc = capture.output(rlang::as_function(.f)(),file=NULL) |>
                     stringr::str_c(collapse="\n"))
  
  # Determine if we need to create or fetch the conversation history
  if (is.null(.c)) {
    if (.auto_save) {
      # Generate a new identifier if autosave is enabled and no identifier is provided
      current_convos <- ls(envir = parent.frame()) |> 
        purrr::keep(~stringr::str_detect(., "convo_as"))
      if (length(current_convos) == 0) {
        c_number <- 0
      } else {
        c_number <- current_convos |> 
          stringr::str_extract("\\d+") |> 
          as.numeric() |> 
          max() + 1
      }
      .c <- glue::glue("convo_as{c_number}")
      message_history <- list()
    } else {
    # No autosave and a NULL .c leads to a warning and an empty conversation is setup
      if(.return_history!=TRUE){ warning("No conversation identifier provided and autosave is not enabled. Starting a temporary conversation.")}
      .c <- "temporary_convo"
      message_history <- list()
    }
  } else if (is.numeric(.c)) {
    # Convert numeric .c into a named variable
    .c <- glue::glue("convo_as{.c}")
    message_history <- get(.c, envir = parent.frame())
  } else if (exists(.c, envir = parent.frame())){
    #If its a string and exsists we get the current object from the parent envir
    message_history <- get(.c, envir = parent.frame())
  } else {
    #If c is a string and does not exist we create a new message history
    message_history <- list()
  }
  
  
  #A function to purge metadata from a message history before sending to API
  purge_metadata <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, purge_metadata)  # recursively apply the function to all list elements
      if ("metadata" %in% names(x)) {
        x$metadata <- NULL  # remove the metadata element
      }
    }
    return(x)
  }
  
  
  #Image handling and preparing user content
  if (!is.null(.imagefile) || .capture_plot) {
    if (.capture_plot) {
      # Capturing the current plot to a temporary file
      plot_file <- tempfile(fileext = ".png")
      dev.copy(png, filename = plot_file)
      dev.off()
      .imagefile <- plot_file
    }
    
    # Validate and encode the image file
    valid_types <- c("jpeg", "png")
    file_type <- tools::file_ext(.imagefile)
    if (!(file_type %in% valid_types)) {
      stop("Unsupported file type. Only JPEG and PNG are allowed.")
    }
    media_type <- paste("image", file_type, sep = "/")
    image_data <- base64enc::base64encode(.imagefile)
    
    # Add image content to the user content
    user_content <- list(role = "user",
                         content = list(
                           list(type = "image",
                                source = list(
                                  type = "base64",
                                  media_type = media_type,
                                  data = image_data
                                )),
                           list(type = "text", text = .prompt)
                         ))
  } else {
    # Text only content
    user_content <- list(role = "user", content = list(list(type = "text", text = .prompt)))
  }
  
  # Call the API with the current message
  response <- c(purge_metadata(message_history),list(user_content)) |> 
    anthropic_api_call(.model = "claude-3-opus-20240229",
                       .max_tokens = .max_tokens,
                       .temperature = .temperature,
                       .timeout = .timeout,
                       .system = .system)
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response)
  
  # Extract the content of the assistant's message
  assistant_content <- list(role = response_decoded$role, 
                                            content = 
                                              list(list(type=response_decoded$content$type, 
                                                        text=response_decoded$content$text)
                                              ))
  
  # Append the assistant's message to the history
  message_history <-  c(message_history,list(user_content),list(assistant_content))
  
  # Store message metadata in the conversation object
  metadata <- list(
    id = response_decoded$id,
    type = response_decoded$type,
    role = response_decoded$role,
    model = response_decoded$model,
    input_tokens = response_decoded$usage$input_tokens,
    output_tokens = response_decoded$usage$output_tokens,
    stop_reason = response_decoded$stop_reason
  )
  
  #Add metadata to the message history
  message_history[[length(message_history)]]$metadata <- metadata
  
  # Update the conversation history in the environment if not temporary
  if (.c != "temporary_convo") {
    assign(.c, message_history, envir = parent.frame())
  }
  
  # Optionally print the conversation
  if (.print) {
    out <- purge_metadata(message_history)
    print_conversation(.m=out[seq(length(out)-1L,length(out))])
  }
  
  if(.return_history==TRUE){return(message_history)} 
  invisible(NULL)
}

#' Get multiple choice answers from Claude models
#'
#' This function sends a prompt with multiple choices to the Claude API and retrieves the best answer.
#'
#' @param .prompt A character string representing the question or prompt.
#' @param .choices A character vector of choices for the given prompt.
#' @param .imagefile An image to include into the user prompt
#' @param .capture_plot Should the last plot be captured and sent to Claude? (Default: FALSE)
#' @param .model A character string specifying the Claude model to use (default: "claude-3-opus-20240229").
#' @param .temperature A numeric value controlling the randomness of the generated answer (default: 0.0).
#' @return A character string representing the best answer chosen by the Claude model.
#' @examples
#' mc_answer("What is the capital of France?", c("London", "Paris", "Berlin", "Madrid"))
#' @export
mc_answer <- function(.prompt, 
                      .choices, 
                      .imagefile = NULL,
                      .capture_plot = FALSE,
                      .model = "claude-3-opus-20240229", 
                      .temperature = 0.0) {
  
  # Validate input prompt and choices
  if (!is.character(.prompt) || length(.prompt) == 0) {
    stop("Prompt must be a non-empty string.")
  }
  if (!is.character(.choices) || length(.choices) == 0) {
    stop("Choices must be a non-empty character vector.")
  }
  
  # Format the prompt and choices
  formatted_prompt <- glue::glue("{.prompt} ({paste(LETTERS[seq_along(.choices)], collapse = ', ')})")
  formatted_choices <- paste("(", paste(LETTERS[seq_along(.choices)],
                                        .choices, sep = ") ", 
                                        collapse = " ("), ")", sep = "")
  message_text <- glue::glue("{formatted_prompt}\n{formatted_choices}")
  
  
  # Image handling and preparing user content
  if (!is.null(.imagefile) || .capture_plot) {
    if (.capture_plot) {
      # Capturing the current plot to a temporary file
      plot_file <- tempfile(fileext = ".png")
      dev.copy(png, filename = plot_file)
      dev.off()
      .imagefile <- plot_file
    }
    
    # Validate and encode the image file
    valid_types <- c("jpeg", "png")
    file_type <- tools::file_ext(.imagefile)
    if (!(file_type %in% valid_types)) {
      stop("Unsupported file type. Only JPEG and PNG are allowed.")
    }
    media_type <- paste("image", file_type, sep = "/")
    image_data <- base64enc::base64encode(.imagefile)
    
    # Add image content to the user content
    user_content <- list(role = "user",
                         content = list(
                           list(type = "image",
                                source = list(
                                  type = "base64",
                                  media_type = media_type,
                                  data = image_data
                                )),
                           list(type = "text", text = message_text)
                         ))
  } else {
    # Text only content
    user_content <- list(role = "user", content = list(list(type = "text", text = message_text)))
  }
  
  # Create the message history
  message <- list(
    user_content,
    list(role = "assistant", content = list(list(type = "text", text = "The best answer is (")))
  )
  
  # Call the API with the message history
  response <- anthropic_api_call(message, 
                                 .model = .model, 
                                 .max_tokens = 1, 
                                 .temperature = .temperature)
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response)
  
  # Extract the answer from the assistant's message
  answer <- response_decoded$content$text
  
  # Get the actual string from the choices as output
  out <- .choices[which(LETTERS == answer)]
  
  # Return the answer
  return(out)
}
