# BSD_2_clause

#' Wrap degrade functions of split PDF files
#'
#' @param pdf Path to the PDF that was split and is to be OCR'd
#' @return A data.frame with infile, outfile, and exit status
#' @export
#' @examples
#' # TBD
degrade_pages <- function(pdf, pages, deg_fn, ...) {
  file_base <- get_file_base(pdf)
  pages <- sprintf("%04d", pages)
  files <- paste0("SPLITs/", file_base, "/", file_base, "_", pages, ".pdf")
  res <- lapply(files, FUN = deg_fn, ...)
  res <- dplyr::rbind_all(res)
  return(res)
}

#' Degrade PDF quality by manipulating pixel density
#'
#' Low-resolution image-based PDFs are particularly susceptible to OCR errors.
#' By simulating low-res PDFs from text-embedded PDFs we may find patterns of
#' OCR errors that improve post-OCR correction.
#'
#' @param file Path to a text-embedded PDF to degrade
#' @param density Pixel density (dpi) to render; integer
#' @return A data.frame with the in and out file names and exit status
#' @seealso \link{degrade_rotate} \link{degrade_fax} \link{degrade_complex} \link{degrade_blur}
#' @export
#' @examples
#' # degrade_density("PDFs/good.pdf", 150)
degrade_density <- function(file, density) {
  file_base <- get_file_base(file)
  suffix <- paste0("_dens", density, ".png")
  outf <- gsub(file, pattern = "\\.pdf$", replacement = suffix)
  outf <- gsub(outf, pattern = "SPLITs", replacement = "IMGs")
  cmd <- paste0("gs -q -dNOPAUSE -sDEVICE=pnggray -r", density,
               " -o %stdout ", file, " | convert -background transparent - ",
               outf)
  res <- system(cmd, intern = TRUE)
  if(identical(res, character(0))) res <- 0
  res_df <- data.frame(infile = file,
                       outfile = outf,
                       exit_status = res,
                       stringsAsFactors = FALSE)
  return(res_df)
}

#' Degrade PDF quality by simulating a fax
#'
#' Low-resolution image-based PDFs are particularly susceptible to OCR errors.
#' By simulating a fax from text-embedded PDFs we may find patterns of
#' OCR errors that improve post-OCR correction.
#'
#' @param file Path to a text-embedded PDF to degrade
#' @return A data.frame with the in and out file names and exit status
#' @seealso \link{degrade_density} \link{degrade_rotate} \link{degrade_complex} \link{degrade_blur}
#' @export
#' @examples
#' # degrade_fax("good.pdf")
degrade_fax <- function(file) {
  suffix = paste0("_fax.tif")
  outf <- gsub(file, pattern = "\\.pdf$", replacement = suffix)
  outf <- gsub(outf, pattern = "SPLITs", replacement = "IMGs")
  cmd <- paste0("gs -q -dNOPAUSE -sDEVICE=tiffg4 -o ", outf, " ",
                file, " -c stop")
  res <- system(cmd, intern = TRUE)
  if(identical(res, character(0))) res <- 0
  res_df <- data.frame(infile = file,
                       outfile = outf,
                       exit_status = res,
                       stringsAsFactors = FALSE)
  return(res_df)
}

#' Degrade PDF quality by simulating page rotation
#'
#' Low-resolution image-based PDFs are particularly susceptible to OCR errors.
#' By simulating page rotation from text-embedded PDFs we may find patterns of
#' OCR errors that improve post-OCR correction.
#'
#' @param file Path to a text-embedded PDF to degrade
#' @param rotate Degrees to rotate the pdf; usual range 0.5 to 5; float
#' @return A data.frame with the in and out file names and exit status
#' @seealso \link{degrade_density} \link{degrade_fax} \link{degrade_complex} \link{degrade_blur}
#' @importFrom stringr str_replace
#' @export
#' @examples
#' # degrade_rotate("good.pdf", 0.5)
degrade_rotate <- function(file, rotate) {
  suffix = paste0("_rot", rotate, ".png")
  outf <- stringr::str_replace(file, pattern = "\\.pdf$", replacement = suffix)
  outf <- stringr::str_replace(outf, pattern = "SPLITs", replacement = "IMGs")
  cmd <- paste0("gs -q -dNOPAUSE -sDEVICE=pnggray -r300 -o %stdout ",
                file, " | convert -background transparent -rotate ", rotate,
                " - ", outf)
  res <- system(cmd, intern = TRUE)
  if(identical(res, character(0))) res <- 0
  res_df <- data.frame(infile = file,
                       outfile = outf,
                       exit_status = res,
                       stringsAsFactors = FALSE)
  return(res_df)
}

#' Degrade PDF quality by simulating blurred text
#'
#' Low-resolution image-based PDFs are particularly susceptible to OCR errors.
#' By simulating blurred text from text-embedded PDFs we may find patterns of
#' OCR errors that improve post-OCR correction.
#'
#' @param file Path to a text-embedded PDF to degrade
#' @param blur Mean and s.d. blur in format 'm'x's', e.g., 2x2
#' @return A data.frame with the in and out file names and exit status
#' @seealso \link{degrade_density} \link{degrade_fax} \link{degrade_complex} \link{degrade_rotate}
#' @importFrom stringr str_replace
#' @export
#' @examples
#' # degrade_blur("good.pdf", 2x2)
degrade_blur <- function(file, blur) {
  suffix = paste0("_blur", blur, ".png")
  outf <- stringr::str_replace(file, pattern = "\\.pdf$", replacement = suffix)
  outf <- stringr::str_replace(outf, pattern = "SPLITs", replacement = "IMGs")
  cmd <- paste0("gs -q -dNOPAUSE -sDEVICE=pnggray -r300 -o %stdout ",
                file, " | convert -background transparent -blur ", blur,
                " - ", outf)
  res <- system(cmd, intern = TRUE)
  if(identical(res, character(0))) res <- 0
  res_df <- data.frame(infile = file,
                       outfile = outf,
                       exit_status = res,
                       stringsAsFactors = FALSE)
  return(res_df)
}

#' Degrade PDF quality by combining degradation parameters
#'
#' Low-resolution image-based PDFs are particularly susceptible to OCR errors.
#' By simulating several effects from text-embedded PDFs we may find patterns of
#' OCR errors that improve post-OCR correction.
#'
#' @param file Path to a text-embedded PDF to degrade
#' @return A data.frame with the in and out file names and exit status
#' @seealso \link{degrade_density} \link{degrade_fax} \link{degrade_blur} \link{degrade_rotate}
#' @importFrom stringr str_replace
#' @export
#' @examples
#' # degrade_complex("good.pdf")
degrade_complex <- function(file) {
  suffix = paste0("_complex.png")
  outf <- stringr::str_replace(file, pattern = "\\.pdf$", replacement = suffix)
  outf <- stringr::str_replace(outf, pattern = "SPLITs", replacement = "IMGs")
  cmd <- paste0("gs -q -dNOPAUSE -sDEVICE=pnggray -r300 -o %stdout ", file,
                " | convert -background transparent -blur 2x2 -rotate 1",
                " -density 150",
                " - ", outf)
  res <- system(cmd, intern = TRUE)
  if(identical(res, character(0))) res <- 0
  res_df <- data.frame(infile = file,
                       outfile = outf,
                       exit_status = res,
                       stringsAsFactors = FALSE)
  return(res_df)
}

#' Simulate degraded PDFs from an input PDF
#'
#' @param file Path to the PDF to be degraded
#' @export
simulate_degrade_set <- function(file) {
  DENS <- c(100, 150, 200, 300)
  ROT <- c(0.5, 1, 2)
  BLUR <- c("1x1", "2x2", "5x2")
  RAND <- c("dens", "rot", "blur")

  sims <- c()
  for(i in 1:4) {
    cur_deg <- sample(RAND, 1)
    if(cur_deg == "dens") {
      cur_amt <- sample(DENS, 1)
      cmd <- paste0("degrade_density('", file, "', ", cur_amt, ")")
    } else if(cur_deg == "rot") {
      cur_amt <- sample(ROT, 1)
      cmd <- paste0("degrade_rotate('", file, "', ", cur_amt, ")")
    } else if(cur_deg == "blur") {
      cur_amt <- sample(BLUR, 1)
      cmd <- paste0("degrade_blur('", file, "', '", cur_amt, "')")
    }
    sims <- c(sims, cmd)
  }
  cmd1 <- paste0("degrade_fax('", file, "')")
  cmd2 <- paste0("degrade_complex('", file, "')")
  sims <- c(sims, cmd1, cmd2)
  return(sims)
}

#' Simulate degraded PDFs from a set of input PDFs
#'
#' @param files List or vector of PDFs to be degraded
#' @export
batch_simulate_degrade_set <- function(files) {
  cmds <- lapply(files, FUN = simulate_degrade_set)
  res <- lapply(cmds, FUN = function(x) eval(parse(text = x)))
}

