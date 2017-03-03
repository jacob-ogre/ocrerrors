# BSD_2_clause

#' Get a set of n-grams from text
#'
#' @param txt Character string from which n-grams are extracted
#' @param from The length of smallest n-gram [1]
#' @param to The length of largest n-gram [5]
#' @return A data.frame of the ngrams and their freq in txt
#' @importFrom tm removePunctuation
#' @importFrom ngram ngram
#' @export
#' @examples
#' # res <- get_ngrams(text, from = 1, to = 3)
get_ngrams <- function(txt, from = 1, to = 5) {
  txt <- tm::removePunctuation(txt)
  txt <- tolower(txt)

  res <- data.frame(ngrams = c(), freq = c(), prop = c())
  for(i in from:to) {
    ng <- ngram::ngram(txt, n = i)
    ng_tab <- ngram::get.phrasetable(ng)
    res <- rbind(res, ng_tab)
  }
  return(res)
}

#' Get a n-grams from one or more texts
#'
#' @param txts A list or vector of char strings from which n-grams are extracted
#' @return A data.frame of the ngrams and their freq across txts
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' # res <- batch_get_ngrams(c(text, from = 1, to = 3)
batch_get_ngrams <- function(txts, from = 1, to = 5) {
  ngs <- lapply(txts, FUN = get_ngrams, from = from, to = to)
  ngs <- dplyr::bind_rows(ngs)
  res <- tapply(ngs$freq,
                INDEX = ngs$ngrams,
                FUN = sum,
                na.rm = TRUE)
  res <- data.frame(ngram = names(res),
                    freq = as.vector(res),
                    stringsAsFactors = FALSE)
  res <- res[order(-res$freq), ]
  return(res)
}

#' Return a df with counts of all characters in df
#'
#' @export
char_ngrams <- function(df) {
  message("Not yet enabled.")
}

#' Get ngrams and counts for bad and gold strings
#'
#' @param bad Text from OCR of same document as gold
#' @param gold Text extracted from embedded text layer of PDF
#' @param n Number of n-grams to find
#' @importFrom ngram ngram get.phrasetable
#' @importFrom dplyr full_join
#' @importFrom tm removePunctuation
#' @export
get_bg_1grams <- function(bad, gold, n = 1) {
  clean <- function(x) {
    x <- tm::removePunctuation(x, preserve_intra_word_dashes = TRUE)
    x <- tolower(x)
    return(x)
  }

  b_1grams <- ngram::ngram(clean(bad), n = n)
  b_1grams <- ngram::get.phrasetable(b_1grams)
  g_1grams <- ngram::ngram(clean(gold), n = n)
  g_1grams <- ngram::get.phrasetable(g_1grams)
  bg_1gram <- dplyr::full_join(b_1grams, g_1grams, by = "ngrams")
  bg_1gram <- data.frame(word = stringr::str_trim(bg_1gram$ngrams),
                         n_bad = bg_1gram$freq.x,
                         n_gld = bg_1gram$freq.y,
                         stringsAsFactors = FALSE)
  return(bg_1gram)
}
