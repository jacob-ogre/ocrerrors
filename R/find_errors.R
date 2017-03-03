# BSD_2_clause

#' Find errors from OCR by comparing to gold standard
#'
#' @param bad A character vector of the text from OCR
#' @param gold A character vector of the embedded text
#' @export
find_errors <- function(bad, gold) {
  bg_1grams <- get_bg_1grams(bad, gold)
  bg_delta <- get_delta_words(bg_1grams)
  bg_delta <- label_delta_words(bg_delta)
  dist_mat <- get_dist_mat(bg_delta[bg_delta$cor_err == "error",]$word,
                           bg_delta[bg_delta$cor_err == "correct",]$word)
  min_dist <- find_min_dists(dist_mat)
  return(min_dist)
  names(min_dist) <- c("word", "good", "dist")
  result <- dplyr::left_join(min_dist, bg_delta, by = "word")
  result <- dplyr::select(result, word:n_bad)
  # return(list(delta = bg_delta, dist = min_dist))
  # return(result)
}

#' Get words with difference frequencies between bad and gold texts
#'
#' @param df A data.frame of bad and gold ngrams with frequencies each
#' @export
get_delta_words <- function(df) {
  res <- dplyr::filter(df, n_bad != n_gld | is.na(n_bad) | is.na(n_gld))
  return(res)
}

#' Label words as correct or errors
#'
#' @param df A data.frame of bad and gold ngrams with frequencies each
#' @export
label_delta_words <- function(df) {
  df$cor_err <- ifelse(is.na(df$n_gld) | (!is.na(df$n_bad) & df$n_gld < df$n_bad),
                       "error",
                       "correct")
  return(df)
}

#' Return a matrix of optimal string alignment distances for bad, gold words
#'
#' @param bad A character vector of the text from OCR
#' @param gold A character vector of the embedded text
#' @export
get_dist_mat <- function(bad, good) {
  mat <- stringdist::stringdistmatrix(bad, good)
  row.names(mat) <- bad
  colnames(mat) <- good
  return(mat)
}

#' Find the minimum string edit for each bad word
#'
#' @param mat A matrix of edit distances
#' @param gold A character vector of the embedded text
#' @export
find_min_dists <- function(mat) {
  mat$bad <- rownames(mat)
  tidy <- tidyr::gather(mat, "bad")
  names(tidy) <- c("bad", "good", "dist")
  min_vals <- tapply(tidy$dist, INDEX = tidy$bad, FUN = min, na.rm = TRUE)
  mins <- data.frame(bad = c(), good = c(), dist = c(),
                     stringsAsFactors = FALSE)
  for(i in names(min_vals)) {
    sub <- dplyr::filter(tidy, bad == i & dist == min_vals[[i]])
    mins <- rbind(mins, sub)
  }
  return(mins)
}

