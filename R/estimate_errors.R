# BSD_2_clause

#' Use hunspell to find errors
#'
#' @param f Path to a PDF to be checked
#' @param save Whether to save the results as they are completed
#' @return A 1-row \link[dplyr]{data_frame} with four variables:
#'   \itemize{
#'     \item file (path)
#'     \item n_word (in the file)
#'     \item n_miss (# 'bad' words in the file)
#'     \item missed ('bad' words in the file)
#'   }
#' @export
hunspell_errors <- function(f, save = TRUE) {
  txt <- try(paste(pdftools::pdf_text(f), collapse = " "), silent = TRUE)
  if(class(txt) == "try-error") {
    res <- dplyr::data_frame(file = f,
                             n_word = NA,
                             n_uniq = NA,
                             n_miss = NA,
                             missed = NA)
  }
  txt <- gsub(txt, pattern = "\n", replacement = " ")
  unigram <- tokenizers::tokenize_words(txt)[[1]]
  uniq <- unique(unigram)
  no_hun <- unlist(hunspell::hunspell(uniq))
  res <- dplyr::data_frame(
    file = f,
    n_word = length(unigram),
    n_uniq = length(uniq),
    n_miss = length(unique(unlist(no_hun))),
    missed = list(unique(unlist(no_hun)))
  )
  if(save) {
    outf <- paste0(f, "_hun.rda")
    saveRDS(res, file = outf)
  }
  return(res)
}
