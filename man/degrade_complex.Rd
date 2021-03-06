% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/degrade.R
\name{degrade_complex}
\alias{degrade_complex}
\title{Degrade PDF quality by combining degradation parameters}
\usage{
degrade_complex(file)
}
\arguments{
\item{file}{Path to a text-embedded PDF to degrade}
}
\value{
A data.frame with the in and out file names and exit status
}
\description{
Low-resolution image-based PDFs are particularly susceptible to OCR errors.
By simulating several effects from text-embedded PDFs we may find patterns of
OCR errors that improve post-OCR correction.
}
\examples{
# degrade_complex("good.pdf")
}
\seealso{
\link{degrade_density} \link{degrade_fax} \link{degrade_blur} \link{degrade_rotate}
}

