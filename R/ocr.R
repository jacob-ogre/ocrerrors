# BSD_2_clause

#' Perform optical character recognition with tesseract
#'
#' Uses Tesseract (which must be installed and on the $PATH) to perform optical
#' character recognition (OCR) on a file.
#'
#' @param file Path to file to be OCR'd
#' @param errfile The file to which Tesseract STDERR is written
#' @return The path to the OCR'd text file
#' @seealso \link{extract_text}
#' @export
#' @examples
#' # res <- tess_ocr("test.pdf")
tess_ocr <- function(file, errfile) {
  outbase <- gsub(file, pattern = "IMGs", replacement = "TXTs")
  outbase <- gsub(outbase, pattern = "\\.png|\\.tif", replacement = "")
  if(!file.exists(paste0(outbase, ".txt"))) {
    cmd <- paste0("tesseract ", file, " ", outbase, " -l eng jwmtest &> ", errfile)
    res <- system(cmd, intern = TRUE)
    if(identical(res, character(0))) {
      return(0)
    } else {
      return(res)
    }
  } else {
    return(paste0(outbase, ".txt already exists."))
  }
}

#' Wrap optical character recognition around a set of files
#'
#' Uses Tesseract (which must be installed and on the $PATH) to perform optical
#' character recognition (OCR) on a file.
#'
#' @param pdf Path to the PDF file to be OCR'd
#' @param pages A vector of pages with embedded text in gold standard
#' @param ext The file extension to be found, either png or tif [png]
#' @param errfile The file to which Tesseract STDERR is written
#' @return A data.frame with
#' @seealso \link{extract_text}
#' @export
#' @examples
#' # res <- tess_ocr("test.pdf")
ocr_pages <- function(pdf, pages, ext = "png", errfile) {
  file_base <- get_file_base(pdf)
  pages <- sprintf("%04d", pages)
  imgs <- list.files(paste0("IMGs/", file_base))
  files <- paste0("IMGs/", file_base, "/", file_base, "_",  pages,
                  "_", degrade, ".", ext)
  outs <- gsub(files, pattern = "IMGs", replacement = "TXTs")
  outs <- gsub(outs, pattern = "png$|tif$", replacement = "txt")
  res <- lapply(files, FUN = tess_ocr, errfile)
  return(res)
}
