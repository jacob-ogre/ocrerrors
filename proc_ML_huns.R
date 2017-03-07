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

huns <- readRDS("~/Downloads/huns_2017-03-07.rds")

names(ML)
na_ML <- filter(ML, is.na(count))
length(unique(na_ML$text_md5))

ML <- filter(ML, !is.na(count))
ML <- filter(ML, tag %in% c("PERSON", "ORGANIZATION", "LOCATION"))
ML_uniq <- unique(ML$entity)
ML_u_tok <- unlist(tokenize_words(ML_uniq))
ML_uniq <- unique(ML_u_tok)

no_ner <- parallel::mclapply(
  seq_along(huns$missed),
  function(x) huns$missed[[x]][!(huns$missed[[x]] %in% ML_uniq)],
  mc.preschedule = TRUE,
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

summary(huns$pct_bad_ori)
summary(huns$pct_bad_ner)
summary(huns$pct_bad_spp)
summary(huns$pct_bad_mis)


saveRDS(huns, "~/Downloads/ESAdocs_misspell.rds")
saveRDS(no_spp, "~/Downloads/ESAdocs_post_spp_list.rds")
saveRDS(no_misc, "~/Downloads/ESAdocs_post_misc_list.rds")

#############
# Some plots!
#############

COLS <- substr(viridis(7), 1, 7)

pcts <- data_frame(
  pctg = c(100 * huns$pct_bad_ori,
           100 * huns$pct_bad_ner,
           100 * huns$pct_bad_spp,
           100 * huns$pct_bad_mis),
  type = c(rep("Hunspell", length(huns$pct_bad_ori)),
           rep("+NER", length(huns$pct_bad_ner)),
           rep("+ITIS", length(huns$pct_bad_spp)),
           rep("+Misc.", length(huns$pct_bad_mis)))
)

ggplot(data = pcts, aes(x = pctg, fill = type)) +
  geom_density(color = NA) +
  scale_fill_viridis(4, alpha = 0.5, discrete = TRUE,
                     guide = guide_legend(title = "word filter"),
                     breaks = c("Hunspell",
                                "+NER",
                                "+ITIS",
                                "+Misc.")) +
  labs(x = "Percent word error") +
  theme_hc()

ggplot(data = pcts, aes(x = pctg, fill = type)) +
  geom_density(color = NA) +
  scale_fill_viridis(4, alpha = 0.5, discrete = TRUE,
                     guide = guide_legend(title = "word filter"),
                     breaks = c("Hunspell",
                                "+NER",
                                "+ITIS",
                                "+Misc.")) +
  xlim(0, 20) +
  labs(x = "Percent word error") +
  theme_hc()


huns$chg_ner_ori <- 100 * huns$pct_bad_ner / huns$pct_bad_ori
huns$chg_spp_ner <- 100 * huns$pct_bad_spp / huns$pct_bad_ner
huns$chg_spp_ori <- 100 * huns$pct_bad_spp / huns$pct_bad_ner
huns$chg_mis_spp <- 100 * huns$pct_bad_mis / huns$pct_bad_spp

chng <- data_frame(
  change = c(huns$chg_ner_ori, huns$chg_spp_ner, huns$chg_mis_spp),
  type = c(rep("Hunspell:NER", length(huns$chg_ner_ori)),
           rep("NER:ITIS", length(huns$chg_spp_ner)),
           rep("ITIS:misc.", length(huns$chg_mis_spp)))
)

ggplot(chng, aes(x = 100 - change, fill = type)) +
  geom_density(color = NA) +
  scale_fill_viridis(3, alpha = 0.5, discrete = TRUE,
                     guide = guide_legend(title = "Transition"),
                     breaks = c("Hunspell:NER",
                                "NER:ITIS",
                                "ITIS:misc.")) +
  labs(x = "Percent error reduction") +
  theme_hc()

ggplot(huns, aes(x = pct_bad_ori, y = pct_bad_mis)) +
  geom_point(size = 4, alpha = 0.2) +
  theme_hc()

low_end <- filter(huns, pct_bad_ori < 0.3 & pct_bad_ner < 0.3 & pct_bad_spp < 0.3)

