% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prompt_library.R
\name{rp}
\alias{rp}
\title{Rephrase a text with a built-in prompt library}
\usage{
rp(
  .text,
  .versions = 3,
  .prompt_type = "better",
  .word_count = NULL,
  .sound_econ = FALSE
)
}
\arguments{
\item{.text}{A text to be rephrased}

\item{.versions}{How many versions should R produce?}

\item{.prompt_type}{How should ChatGPT improve the text? The options are better, readable, concise, engaging, professional, persuasive and elegant}

\item{.word_count}{The maximum number of words ChatGPT should aim for}

\item{.sound_econ}{Should the text sound like a QJE paragraph (does only work semi-well)}
}
\value{
A ChatGPT message list
}
\description{
This function sends a text with a rephrase prompt to chatgpt to make a text better
}
