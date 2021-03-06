% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_errors.R
\name{hunspell_errors}
\alias{hunspell_errors}
\title{Use hunspell to find errors}
\usage{
hunspell_errors(f)
}
\arguments{
\item{f}{Path to a PDF to be checked}
}
\value{
A 1-row \link[dplyr]{data_frame} with four variables:
  \itemize{
    \item file (path)
    \item n_word (in the file)
    \item n_miss (# 'bad' words in the file)
    \item missed ('bad' words in the file)
  }
}
\description{
Use hunspell to find errors
}

