library(dplyr)
library(ocrerrors)

fils <- list.files("ESAdocs_on_GCE", full.names = TRUE, recursive = TRUE)
huns <- parallel::mclapply(
  fils,
  hunspell_errors,
  save = FALSE,
  mc.preschedule = FALSE,
  mc.cores = 14
)
huns_df <- dplyr::bind_rows(huns)
save(huns_df, file = "/datadrive/data/ESAdocs_on_GCE/huns_df_p1.rda")

done <- unique(huns_df$file)
new_fils <- fils[!(fils %in% done)]

huns <- parallel::mclapply(
  new_fils[1:80],
  hunspell_errors,
  mc.preschedule = FALSE,
  mc.cores = 8
)
huns_df2 <- dplyr::bind_rows(huns)
