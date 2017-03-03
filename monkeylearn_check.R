############################################################################
#
############################################################################
#
# After a lot of work and a lot of trial-and-error, MonkeyLearn is just too
# fragile for the task of NER with all of ESAdocs. Will instead work on a hackey
# solution that uses coreNLP...
#
############################################################################

library(digest)
library(dplyr)
library(monkeylearn)
library(pdftools)

break_up_big <- function(x, fname, md5, chunk = 400000) {
  len <- nchar(x)
  strt <- seq(1, len, chunk)
  # print(paste("strt:", paste(strt, collapse = " ")))
  ends <- seq(chunk, len, chunk)
  if(max(ends) > len) ends[length(ends)] <- len
  print(paste("ends:", paste(ends, collapse = " ")))
  subs <- substring(x, strt, ends)
  md5s <- unlist(lapply(subs, digest, file = FALSE))
  result <- data_frame(sub_txt = subs, sub_md5 = md5s,
                       file_md5 = md5, file = fname)
  return(result)
}

proc_fils <- function(files, done) {
  get_txt <- function(f) {
    res <- try(suppressMessages(pdf_info(f)), silent = TRUE)
    if(class(res) == "try-error") return(" ")
    return(paste(suppressMessages(pdf_text(f)), collapse = " "))
  }
  texts <- lapply(files, get_txt)
  texts <- lapply(texts, enc2utf8)
  print("Texts acquired.")
  texts <- lapply(texts, gsub, pattern = "\n", replacement = " ")
  md5s <- unlist(lapply(texts, digest, file = FALSE))
  f_md5 <- data_frame(file = files, md5 = md5s)
  cur_sm <- sm_md5 <- sm_nam <- too_big <- big_nam <- big_md5 <- c()
  for(t in 1:length(texts)) {
    if(nchar(texts[[t]]) < 400000 & !(md5s[t] %in% done)) {
      cur_sm <- c(cur_sm, texts[[t]])
      sm_nam <- c(sm_nam, files[t])
      sm_md5 <- c(sm_md5, md5s[t])
    } else if(nchar(texts[[t]]) > 400000 & !(md5s[t] %in% done)) {
      too_big <- c(too_big, texts[[t]])
      big_nam <- c(big_nam, files[t])
      big_md5 <- c(big_md5, md5s[t])
    }
  }
  if(!is.null(cur_sm)) {
    cur_smalls <- data_frame(txt = cur_sm, file = sm_nam, md5 = sm_md5)
    cur_smalls <- filter(cur_smalls, !(md5 %in% done))
    cur_smalls <- filter(cur_smalls, txt != " ")
    print(paste("dim(cur_smalls):", paste(dim(cur_smalls), collapse = " ")))
    if(length(cur_smalls$txt) > 0) {
      print("Extracting small texts")
      smalls <- try(
        monkeylearn_extract(cur_smalls$txt,
                            extractor_id = "ex_isnnZRbS"), silent = TRUE
      )
      if(class(smalls) != "try-error" & "text_md5" %in% names(smalls)) {
        sm_res <- left_join(smalls, f_md5, by = c("text_md5" = "md5"))
        #count, tag, entity, text_md5, file
      } else {
        print(head(substr(cur_smalls$txt, 1, 1000)))
        sm_res <- data_frame(count = NA, tag = NA, entity = NA,
                             text_md5 = cur_smalls$md5,
                             file = cur_smalls$file)
      }
    }
  }
  # Now process the big docs
  lg_parts <- list()
  print("Splitting up bigs")
  print(paste("Length too_big:", length(too_big)))
  if(length(too_big) > 0) {
    for(b in 1:length(too_big)) {
      lg_parts[[b]] <- break_up_big(too_big[b], big_nam[b], big_md5[b])
    }
    lg_txt <- bind_rows(lg_parts) #sub_txt, sub_md5, file_md5, file_name
    print(paste("dim(lg_txt):", dim(lg_txt)))
    if(length(lg_txt$sub_txt) > 0) {
      print("Extracting large texts")
      sub_res <- list()
      n_chunks <- floor(length(lg_txt$sub_txt) / 200)
      for(j in 0:n_chunks) {
        print(paste("Large chunk", j))
        lo <- (j*200)+1
        hi <- (j+1)*200
        if(hi > length(lg_txt$sub_txt)) hi <- length(lg_txt$sub_txt)
        cur_set <- lg_txt$sub_txt[lo:hi]
        cur_res <- try(monkeylearn_extract(cur_set, extractor_id = "ex_isnnZRbS"))
        if(class(cur_res) != "try-error") {
          sub_res[[j+1]] <- cur_res
        } else {
          print(files)
          print(head(substr(cur_set, 1, 1000)))
          null_res <- data_frame(count = NA, tag = NA, entity = NA,
                                 text_md5 = lg_txt$sub_md5)
          sub_res[[j+1]] <- null_res
        }
        Sys.sleep(65)
    }
    larges <- bind_rows(sub_res) #count, tag, entity, text_md5
      for_join <- select(lg_txt, -sub_txt)
      large_res <- left_join(larges, for_join, by = c("text_md5" = "sub_md5"))
        #count, tag, entity, text_md5, file_md5, file_name
      large_res$text_md5 <- large_res$file_md5
      lg_res <- select(large_res, -file_md5)
    }
  }
  if(exists("sm_res") & exists("lg_res")) {
    print(names(sm_res))
    print(names(lg_res))
    return(rbind(sm_res, lg_res))
  } else if(exists("sm_res")) {
    return(sm_res)
  } else if(exists("lg_res")) {
    return(lg_res)
  } else {
    return(NULL)
  }
}

fils <- list.files("/datadrive/data/ESAdocs_on_GCE",
                   full.names = TRUE,
                   recursive = TRUE)
load("ESAdocs_on_GCE/first_100_ML_p2.rda")
load("ESAdocs_on_GCE/batch_28Feb0545h.rda")
done_set <- rbind(next_batch_ML, part_res, res_01Mar)
done_set <- filter(done_set, !is.na(tag))
done_set <- filter(done_set, tag %in% c("LOCATION", "ORGANIZATION", "PERSON"))

results <- list()
n_chunks <- floor(length(fils) / 200)
# n_chunks <- floor(13581 / 200)
for(i in 0:n_chunks) {
  print(paste("Main chunk", i))
  lo <- (i*200)+1
  hi <- (i+1)*200
  if(hi > length(fils)) hi <- length(fils)
  cur_set <- fils[lo:hi]
  # print(c(lo, hi))
  results[[i+1]] <- proc_fils(cur_set, unique(done_set$text_md5))
  Sys.sleep(65)
}

monkey_res <- bind_rows(results)

## This chunk was used to try to process the docs on a first go-around, but
## proved to have some problems (well, probs in proc_fils). Next block down
## uses the new proc_fils.
# fils <- list.files("/datadrive/data/ESAdocs_on_GCE",
#                    full.names = TRUE,
#                    recursive = TRUE)
# results <- list()
# n_chunks <- length(fils) / 10
# for(i in 0:n_chunks) {
#   lo <- (i*10)+1
#   hi <- (i+1)*10
#   if(hi > length(fils)) hi <- length(fils)
#   cur_set <- fils[lo:hi]
#   results[[i+1]] <- proc_fils(cur_set)
#   Sys.sleep(65)
# }
#
# check_stanford_installed <- function(stanford = stansent::coreNLP_loc(),
#                                      download = stansent::coreNLP_url(), verbose = TRUE){
#
#   if (isTRUE(verbose)) message("\nchecking if coreNLP is installed...\n")
#
#   root <- strsplit(getwd(), "(/|\\\\)+")[[1]][1]
#   out <- file.exists(stanford)
#
#   if (isTRUE(out)) {
#     mess <- paste0("Stanford coreNLP appears to be installed.\n\n",
#                    "...Let the NLP tagging begin!\n\n")
#     if (isTRUE(verbose)) message(mess)
#     return(invisible(NULL))
#   } else {
#     mess <- "Stanford coreNLP does not appear to be installed in root.\nWould you like me to try to install it there?"
#     message(mess)
#
#     ans <- utils::menu(c("Yes", "No"))
#     if (ans == "2") {
#       stop("Please consider installing yourself...\n\nhttp://stanfordnlp.github.io/CoreNLP")
#     } else {
#       message("Let me try...\nHold on.  It's large and may take some time...\n")
#     }
#
#     temp <- tempdir()
#     dest <- "~/Downloads/nlp1/stanford-corenlp-full-2016-10-31.zip"
#     # utils::download.file(download, dest)
#     # utils::unzip(dest, exdir = temp)
#     stan <- gsub("\\.zip$", "", dest)
#     if (!file.exists(stan)) stop(
#       "I let you down :-/\nIt appears the file was not downloaded.\n",
#       "Consider installing yourself via:\n\n",
#       "http://nlp.stanford.edu/software/corenlp.shtml\n\n"
#     )
#     print(c(stan, root))
#     file.copy(stan, file.path(root, "/"), , TRUE)
#     if (file.exists(file.path(root, basename(stan)))) message(
#       "I have done it...\nStanford's coreNLP appears to be installed.\n\n",
#       "...Let the NLP tagging begin!\n\n"
#     )
#   }
# }

