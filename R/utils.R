# BSD_2_clause

#' Clean EOL characters from
#'
#' Removes single EOL sequential characters so that sentences are not split but
#' paragraphs remain separated.
#'
#' @export
#' @examples
#' # create_dirs()
normalize_text <- function(txt) {
  remove_eol <- function(x) {
    res <- gsub(x, pattern = "[a-z ]\n$", replacement)
  }
}

#' Split a PDF into multiple pages
#'
#' Uses \href{https://www.pdflabs.com/docs/pdftk-man-page/}{PDFtk}, which must
#' be on the $PATH, to split the PDF.
#'
#' @param file Path to the PDF to be split
#' @return None
#' @export
#' @examples
#' # split_pdf("PDFs/test.pdf")
split_pdf <- function(file) {
  file_base <- get_file_base(file)
  split_dir <- paste0("SPLITs/", file_base, "/")
  img_dir <- paste0("IMGs/", file_base, "/")
  txt_dir <- paste0("TXTs/", file_base, "/")
  if(!dir.exists(split_dir)) dir.create(split_dir)
  if(!dir.exists(img_dir)) dir.create(img_dir)
  if(!dir.exists(txt_dir)) dir.create(txt_dir)
  cmd <- paste0("pdftk ", file, " burst output ",
                split_dir, file_base, "_%04d.pdf")
  system(cmd, wait = TRUE)
}

#' Create directories for \code{ocrerrs}
#'
#' \code{ocrerrs} package expects particular directories will exist if run
#' in full. This creates those expected directories in the working directory.
#' \describe{
#'   \item{GOLDs}{Contains .txt and .rda of gold-standard text}
#'   \item{SPLITs}{Contains one subdir for each PDF, and subdirs contain
#'   one file per page of the PDF}
#'   \item{IMGs}{Contains one subdir for each PDF, and subdirs contain one .tif
#'   or .png per page of the PDF}
#'   \item{TXTs}{Contains one subdir for each PDF, and subdirs contain one .txt
#'   from OCR of each page of the PDF}
#'   \item{BADs}{Contains .txt of the PDF, from concatenating the files in
#'   TXTs/<pdfname>}
#'   \item{RES}{Contains results files from OCR error-finding}
#' }
#'
#' @return None. Message printed.
#' @export
#' @examples
#' # create_dirs()
create_dirs <- function() {
  if(!dir.exists("GOLDs")) dir.create("GOLDs")
  if(!dir.exists("SPLITs")) dir.create("SPLITs")
  if(!dir.exists("IMGs")) dir.create("IMGs")
  if(!dir.exists("TXTs")) dir.create("TXTs")
  if(!dir.exists("BADs")) dir.create("BADs")
  if(!dir.exists("RES")) dir.create("RES")
  message("Directories are ready.")
}

#' Create a path to which 'gold standard' results are written
#'
#' @param file Path to the PDF to be processed
#' @param ext File extension of file to be written
#' @return The compiled path (character)
#' @export
#' @examples
#' make_gold_path("PDFs/test.pdf", "txt")
#' # GOLDs/test.txt
make_gold_path <- function(file, ext) {
  file_base <- get_file_base(file)
  file_base <- paste0(file_base, ".", ext)
  txt_path <- paste("GOLDs", file_base, sep = "/")
  return(txt_path)
}

#' Return the base name of a file
#'
#' More detailed description of the function
#' @param file Path to the file
#' @return The base name of the file, without directories or file suffix
#' @export
#' @examples
#' get_file_base("data/test.pdf")
#' # test
get_file_base <- function(file) {
  file_base <- strsplit(file, split = "/")
  file_base <- file_base[[1]][length(file_base[[1]])]
  file_base <- gsub(file_base,
                    pattern = "\\.pdf$|\\.png$|\\.rda$|\\.tif$",
                    replacement = "")
  return(file_base)
}
