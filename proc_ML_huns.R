# BSD_2_clause

# Combine hunspell and MonkeyLearn results to get a better idea of the error
# distribution

library(dplyr)
library(ggplot2)
library(ggthemes)
library(parallel)
library(ritis)
library(tokenizers)
library(viridis)

load("~/Downloads/remaining_ML_01Mar2017.rda")
ML <- all_res
rm(all_res)

huns <- readRDS("~/Downloads/huns_df.rds")

names(ML)
na_ML <- filter(ML, is.na(count))
length(unique(na_ML$text_md5))

ML <- filter(ML, !is.na(count))
ML <- filter(ML, tag %in% c("PERSON", "ORGANIZATION", "LOCATION"))
ML_uniq <- unique(ML$entity)
ML_u_tok <- unlist(tokenize_words(ML_uniq))
ML_uniq <- unique(ML_u_tok)

no_ner <- lapply(seq_along(tt), function(x) tt[[x]][!(tt[[x]] %in% ML_uniq)])

no_ner <- parallel::mclapply(
  seq_along(huns$missed),
  function(x) huns$missed[[x]][!(huns$missed[[x]] %in% ML_uniq)],
  mc.preschedule = FALSE,
  mc.cores = 3
)
huns$no_ner <- no_ner
huns$n_bad_filt <- sapply(huns$no_ner, length)

no_spp <- parallel::mclapply(
  seq_along(huns$no_ner),
  function(x) huns$no_ner[[x]][!(huns$no_ner[[x]] %in% uniq_sci_names)],
  mc.preschedule = TRUE,
  mc.cores = 3
)
huns$no_spp <- no_spp
huns$n_no_spp <- sapply(no_spp, length)

remain_bad <- unlist(no_spp)
bad_tab <- table(remain_bad)
bad_mult50 <- names(bad_tab[bad_tab >= 50])
no_misc <- parallel::mclapply(
  seq_along(huns$no_spp),
  function(x) huns$no_spp[[x]][!(huns$no_spp[[x]] %in% bad_mult50)],
  mc.preschedule = TRUE,
  mc.cores = 3
)
huns$no_misc <- no_misc
huns$n_no_misc <- sapply(no_misc, length)

chk_err <- unlist(no_misc)
head(sort(table(chk_err), decreasing = TRUE), 50)

huns$pct_bad_ori <- huns$n_miss / huns$n_uniq
huns$pct_bad_ner <- huns$n_bad_filt / huns$n_uniq
huns$pct_bad_spp <- huns$n_no_spp / huns$n_uniq
huns$pct_bad_mis <- huns$n_no_misc / huns$n_uniq

saveRDS(huns, "~/Downloads/ESAdocs_misspell.rds")
saveRDS(no_spp, "~/Downloads/ESAdocs_post_spp_list.rds")
saveRDS(no_misc, "~/Downloads/ESAdocs_post_misc_list.rds")

#############
# Some plots!
#############

COLS <- substr(viridis(7), 1, 7)

ggplot(data = huns, aes(x = (100 * pct_bad_ori))) +
  geom_density(alpha = 0.8, fill = COLS[1], color = NA) +
  geom_density(aes(x = 100 * pct_bad_ner), alpha = 0.8, fill = COLS[3], color = NA) +
  geom_density(aes(x = 100 * pct_bad_spp), alpha = 0.6, fill = COLS[5], color = NA) +
  geom_density(aes(x = 100 * pct_bad_mis), alpha = 0.6, fill = COLS[7], color = NA) +
  # xlim(0, 66) +
  labs(x = "Word error rate (percent)") +
  theme_hc()

ggplot(data = huns, aes(x = (100 * pct_bad_ori))) +
  geom_density(alpha = 0.8, fill = COLS[1], color = NA) +
  geom_density(aes(x = 100 * pct_bad_ner), alpha = 0.8, fill = COLS[3], color = NA) +
  geom_density(aes(x = 100 * pct_bad_spp), alpha = 0.6, fill = COLS[5], color = NA) +
  geom_density(aes(x = 100 * pct_bad_mis), alpha = 0.6, fill = COLS[7], color = NA) +
  xlim(0, 20) +
  labs(x = "Word error rate (percent)") +
  theme_hc()

ggplot(huns, aes(x = pct_bad_ori, y = pct_bad_mis)) +
  geom_point(size = 4, alpha = 0.2) +
  theme_hc()

low_end <- filter(huns, pct_bad_ori < 0.3 & pct_bad_ner < 0.3 & pct_bad_spp < 0.3)

huns$chg_ner_ori <- 100 * huns$pct_bad_ner / huns$pct_bad_ori
huns$chg_spp_ner <- 100 * huns$pct_bad_spp / huns$pct_bad_ner
huns$chg_spp_ori <- 100 * huns$pct_bad_spp / huns$pct_bad_ner

ggplot(huns, aes(x = 100 * pct_bad_ner / pct_bad_ori)) +
  geom_density(fill = COLS[1], colour = NA, alpha = 0.5) +
  geom_density(aes(100 * pct_bad_spp / pct_bad_ner),
               fill = COLS[3], colour = NA, alpha = 0.7) +
  geom_density(aes(100 * pct_bad_mis / pct_bad_spp),
               fill = COLS[7], colour = NA, alpha = 0.7) +
  labs(x = "Word error improvement (percent)") +
  theme_hc()
