## ----setup, include = FALSE, warning = FALSE----------------------------------
safe_read_csv <- purrr::safely(readr::read_csv)
safe_read_lines <- purrr::safely(readr::read_lines)

#check if all necessary files can be read
all_available <- all(is.null(safe_read_csv("https://github.com/yasche/metamorphr-data/raw/refs/heads/main/RP18/pos/MetaboScape/menadione/menadione.csv", show_col_types = FALSE)$error),
                     is.null(safe_read_csv("https://github.com/yasche/metamorphr-data/raw/refs/heads/main/RP18/pos/MetaboScape/menadione/menadione_metadata.csv", show_col_types = FALSE)$error),
                     is.null(safe_read_lines("https://github.com/yasche/metamorphr-data/raw/refs/heads/main/RP18/pos/MetaboScape/menadione/menadione.gnps.mgf", n_max = 1)$error))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE,
  eval = all_available
)

## ----eval = !all_available, echo = FALSE, comment = NA------------------------
# message("Can't download example files from GitHub. Code in this vignette will not be evaluated.")

## ----lib----------------------------------------------------------------------
library(metamorphr)

## ----read-files, warning=FALSE------------------------------------------------
menadione_ft <- read_featuretable(
  "https://github.com/yasche/metamorphr-data/raw/refs/heads/main/RP18/pos/MetaboScape/menadione/menadione.csv", 
  label_col = 3, 
  metadata_cols = 1:15)

menadione_metadata <- readr::read_csv(
  "https://github.com/yasche/metamorphr-data/raw/refs/heads/main/RP18/pos/MetaboScape/menadione/menadione_metadata.csv", 
  show_col_types = FALSE)

menadione_msn <- read_mgf("https://github.com/yasche/metamorphr-data/raw/refs/heads/main/RP18/pos/MetaboScape/menadione/menadione.gnps.mgf",
                          show_progress = F)

## ----join-files---------------------------------------------------------------
menadione_ft <- menadione_ft %>%
  join_metadata(menadione_metadata) %>%
  dplyr::left_join(menadione_msn, by = "FEATURE_ID")

head(menadione_ft)

## ----init-filter--------------------------------------------------------------
menadione_ft <- menadione_ft %>%
  dplyr::filter(Group != "QC") %>%
  filter_grouped_mv(min_found = 0.75)  %>%
  filter_blank(blank_samples = c("blank", "control"),
               blank_as_group = T,
               group_column = Group,
               min_frac = 3) %>%
  dplyr::filter(Group != "blank")

## ----filter-ms-ms-------------------------------------------------------------
fragments <- c(308.0911,
               179.0485,
               177.0328,
               162.0219,
               130.0499)

menadione_ft %>%
  filter_msn(fragments = fragments, 
             min_found = 2, 
             tolerance = 5, 
             tolerance_type = "ppm", 
             show_progress = F) %>%
  ggplot2::ggplot(ggplot2::aes(Group, Intensity)) +
    ggplot2::geom_point(position = "jitter") +
    ggplot2::facet_wrap(~ Feature) +
    ggplot2::ylim(c(0, NA))

## ----filter-losses------------------------------------------------------------
losses <- c(75.0320,
            129.0426,
            232.0696,
            146.0692,
            249.0961,
            273.0961,
            275.1117,
            305.0682,
            307.0838)

menadione_ft %>%
  msn_calc_nl(m_z_col = PEPMASS) %>%
  filter_neutral_loss(losses = losses, 
                      min_found = 4, 
                      tolerance = 10, 
                      tolerance_type = "ppm",
                      show_progress = F) %>%
  ggplot2::ggplot(ggplot2::aes(Group, Intensity)) +
    ggplot2::geom_point(position = "jitter") +
    ggplot2::facet_wrap(~ Feature) +
    ggplot2::ylim(c(0, NA))

