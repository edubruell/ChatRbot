#' Rephrase a text with a built-in prompt library
#'
#' This function sends a text with a rephrase prompt to chatgpt to make a text better
#'
#' @param .text A text to be rephrased
#' @param .versions How many versions should R produce?
#' @param .prompt_type How should ChatGPT improve the text? The options are better, readable, concise, engaging, professional, persuasive and elegant
#' @param .word_count The maximum number of words ChatGPT should aim for
#' @param .sound_econ Should the text sound like a QJE paragraph (does only work semi-well)

#' @return A ChatGPT message list
#' @export
#'
rp <- function(.text, .versions = 3, .prompt_type = "better", .word_count = NULL,.sound_econ=FALSE) {
  prompt <- switch(.prompt_type,
                   "better" = glue("Can you please rephrase this text to make it better:\n{.text}"),
                   "readable" = glue("Can you please rephrase this text to be more readable:\n{.text}"),
                   "concise" = glue("Can you please rephrase this text to be more concise:\n{.text}"),
                   "engaging" = glue("Can you make this text more engaging:\n{.text}"),
                   "professional" = glue("Can you rephrase this text to make it more professional:\n{.text}"),
                   "persuasive" = glue("Can you rewrite this text to be more persuasive:\n{.text}"),
                   "elegant" = glue("Can you rewrite this text to be more elegant:\n{.text}"),
                   stop("Invalid prompt type.")
  )

  if (!is.null(.word_count)) {
    prompt <- glue("{prompt}\n\nPlease aim for a word count of {.word_count} words.")
  }

  if (.versions > 1) {
    prompt <- glue("{prompt}\n\nPlease give me {.versions} versions of the text below that are more {.prompt_type}.")
  }

  if (.sound_econ) {
    prompt <- glue("{prompt}\n\nThe resulting text should read like a paragraph from a well-written publication in the Quarterly Journal of Economics.")
  }

  chat(.text = prompt,.auto_save_c=FALSE)
}
