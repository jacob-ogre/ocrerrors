% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngrams.R
\name{batch_get_ngrams}
\alias{batch_get_ngrams}
\title{Get a n-grams from one or more texts}
\usage{
batch_get_ngrams(txts, from = 1, to = 5)
}
\arguments{
\item{txts}{A list or vector of char strings from which n-grams are extracted}
}
\value{
A data.frame of the ngrams and their freq across txts
}
\description{
Get a n-grams from one or more texts
}
\examples{
# res <- batch_get_ngrams(c(text, from = 1, to = 3)
}

