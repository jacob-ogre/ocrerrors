# BSD_2_clause

#' Extract text from a PDF with embedded text.
#'
#' Uses \code{pdftools::pdf_text} to get the text layer from PDF `file`, which
#' is used as the 'gold standard' against which OCR'd versions are compared.
#' Checks that the text layer is distilled from the original document rather
#' than a text layer from OCR, e.g., a scanner that OCRs.
#'
#' @param file Path to the PDF to be processed
#' @param write Whether to write the text to file [FALSE]
#' @param save Whether to save the text as a .rda [TRUE]
#' @return List of pages with text layer if layer not from OCR; else NULL
#' @importFrom pdftools pdf_text
#' @export
#' @examples
#' # res <- get_gold("test.pdf", "GOLDs")
get_gold <- function(file, write = FALSE, save = TRUE) {
  if(check_embed(file)) {
    text <- pdftools::pdf_text(file)
    embed_pages <- get_embed_pages(text)
    if(write) write_gold(file, text)
    if(save) save_gold_text(file, text)
    return(embed_pages)
  } else {
    message(paste(file, "has a text layer, but it appears to be from OCR."))
    return(NULL)
  }
}

#' Check if text embed is not from OCR
#'
#' Some PDFs have an embedded text layer that is derived from OCR by the scanner
#' or other equipment that produced the PDF. Such documents will likely have
#' OCR artifacts that will contaminate the 'gold standard' that is needed for
#' error correction. The gold standard texts should only come from PDFs derived
#' directly from the original document (e.g., .docx).
#'
#' @param file Path to a PDF to check for embedding source
#' @return Logical: TRUE if good embed, FALSE if from OCR
#' @seealso \code{pdftools::pdf_info}
#' @export
#' @examples
#' # res <- summarize_gold("test.pdf", text)
check_embed <- function(file) {
  temp <- pdftools::pdf_info(file)
  if(length(temp) > 1) {
    info <- list(temp)
  } else {
    info <- temp
  }
  if(!is.atomic(info[[1]][1]) & !is.na(info[[1]][1])) {
    if(!is.null(info[[1]]$keys)) {
      if(!is.null(info[[1]]$keys$Producer)) {
        if(grepl(info[[1]]$keys$Producer,
                 pattern = "Distiller|Word|Library|Ghost|Acrobat Pro")) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

#' Return a vector of pages with embedded text
#'
#' A PDF with a text layer may also have image-only pages. To find errors, we
#' need to OCR only the (degraded) pages with embedded text because finding
#' differences between the BAD and GOLD versions of the text depends on counts
#' of each word...if OCR picks up words on pages that \code{pdf_text} cannot
#' see because they are in am image, then GOLD != BAD because of different
#' inputs.
#'
#' @param txt An object (list of pages) from \code{pdf_tools::pdf_text}
#' @return A vector of pages with an embedded text layer
#' @export
#' @examples
#' # to be added
get_embed_pages <- function(txt) {
  pages <- seq(1, length(txt))
  empty <- match("", txt)
  embed <- pages[-empty]
  return(embed)
}

#' Write the extracted text to file
#'
#' @param file Path to the PDF to be processed
#' @param gold_path Path to which gold-standard TXT is written
#' @param text Text extracted using \code{pdftools::pdf_text}
#' @return None
#' @export
#' @examples
#' # write_gold_text("test.pdf", "GOLDs", text)
write_gold_text <- function(file, text) {
  if(class(text) == "list") text <- unlist(text)
  txt_path <- make_gold_path(file, "txt")
  write(text, file = txt_path)
}

#' Save the extracted text as a .rda
#'
#' @param file Path to the PDF to be processed
#' @param gold_path Path to which gold-standard TXT is written
#' @param text Text extracted using \code{pdftools::pdf_text}
#' @return None
#' @export
#' @examples
#' # save_gold_text("test.pdf", "GOLDs", text)
save_gold_text <- function(file, text) {
  txt_path <- make_gold_path(file, "rda")
  save(text, file = txt_path)
}

#' Extract text from a set of PDFs with embedded text.
#'
#' @param ... List/vector of paths to PDFs to be processed with \link{get_gold}
#' @param gold_path Path to which gold-standard TXTs are written
#' @return A summary (data.frame) of the files
#' @importFrom pdftools pdf_text
#' @seealso \link{get_gold}
#' @export
#' @examples
#' # res <- batch_get_gold(list.files("PDFs", pattern = "pdf"), "GOLDs")
batch_get_gold <- function(files, gold_path) {
  res <- lapply(files, FUN = get_gold, gold_path = gold_path)
  res <- dplyr::rbind_all(res)
  return(res)
}

#' Summarize the text from a gold-standard PDF
#'
#' More detailed description of the function
#' @param fil Name of the PDF from which text was extracted
#' @param txt List from \code{pdftools::pdf_text} with text from PDF
#' @return A data.frame with # pages, # lines, and # words
#' @importFrom quanteda tokenize
#' @export
#' @examples
#' # res <- summarize_gold("test.pdf", text)
summarize_gold <- function(fil, txt) {
  n_pages <- length(txt)
  tokens <- quanteda::tokenize(unlist(txt))
  n_lines <- length(tokens)
  n_words <- length(unlist(tokens))
  res <- data.frame(file = fil,
                    n_pages = n_pages,
                    n_lines = n_lines,
                    n_words = n_words,
                    stringsAsFactors = FALSE)
  return(res)
}
