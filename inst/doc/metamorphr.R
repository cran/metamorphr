## ----setup, include = FALSE, warning = FALSE----------------------------------
safe_read_csv <- purrr::safely(readr::read_csv)
#check if all necessary files can be read

all_available <- all(is.null(safe_read_csv("https://raw.githubusercontent.com/yasche/metamorphr-data/main/RP18/pos/MetaboScape/mevastatin/mevastatin.csv", show_col_types = FALSE)$error),
                     is.null(safe_read_csv("https://raw.githubusercontent.com/yasche/metamorphr-data/main/RP18/pos/MetaboScape/mevastatin/mevastatin_metadata.csv", show_col_types = FALSE)$error))

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

## ----whole-thing, warning=FALSE-----------------------------------------------
mevastatin_ft <- read_featuretable(
  "https://raw.githubusercontent.com/yasche/metamorphr-data/main/RP18/pos/MetaboScape/mevastatin/mevastatin.csv", 
  label_col = 3, 
  metadata_cols = 1:15)

mevastatin_metadata <- readr::read_csv(
  "https://raw.githubusercontent.com/yasche/metamorphr-data/main/RP18/pos/MetaboScape/mevastatin/mevastatin_metadata.csv", 
  show_col_types = FALSE)

mevastatin_ft %>%
  join_metadata(mevastatin_metadata) %>%
  filter_grouped_mv(min_found = 0.75,
                    group_column = Group) %>%
  filter_blank(blank_samples = "blank", 
               blank_as_group = T,
               group_column = Group) %>%
  filter_cv(reference_samples = "QC",
            ref_as_group = T,
            group_column = Group) %>%
  dplyr::filter(Group != "blank") %>%
  impute_knn() %>% 
  normalize_quantile_smooth() %>%
  collapse_mean(sample_metadata_cols = "Group",
                feature_metadata_cols = "Feature") %>%
  plot_volcano(group_column = Group,
               name_column = Feature, 
               groups_to_compare = c("control", "mevastatin")) +
    ggplot2::geom_hline(yintercept = -log10(0.05),
                        color = "grey40",
                        linetype = 2) +
    ggplot2::geom_vline(xintercept = c(- 1, 1),
                        color = "grey40",
                        linetype = 2) +
    ggplot2::theme_bw()

## ----read-ft------------------------------------------------------------------
mevastatin_ft <- read_featuretable(
  "https://raw.githubusercontent.com/yasche/metamorphr-data/main/RP18/pos/MetaboScape/mevastatin/mevastatin.csv", 
  label_col = 3, 
  metadata_cols = 1:15)

## ----ft-head------------------------------------------------------------------
head(mevastatin_ft)

## ----md-example, eval=FALSE---------------------------------------------------
# mevastatin_ft %>%
#   create_metadata_skeleton() %>%
#   readr::write_csv(file = "some/path/menadione_metadata.csv")
# 
# mevastatin_metadata <- readr::read_csv("some/path/menadione_metadata.csv")

## ----read-metadata------------------------------------------------------------
mevastatin_metadata <- readr::read_csv(
  "https://raw.githubusercontent.com/yasche/metamorphr-data/main/RP18/pos/MetaboScape/mevastatin/mevastatin_metadata.csv", 
  show_col_types = FALSE)

## ----show-metadata------------------------------------------------------------
head(mevastatin_metadata)

## ----join-metadata------------------------------------------------------------
mevastatin_ft <- join_metadata(mevastatin_ft, mevastatin_metadata)

## ----filter-features----------------------------------------------------------
mevastatin_ft <- mevastatin_ft %>%
  filter_grouped_mv(min_found = 0.75,
                    group_column = Group) %>%
  filter_blank(blank_samples = "blank", 
               blank_as_group = T,
               group_column = Group) %>%
  filter_cv(reference_samples = "QC",
            ref_as_group = T,
            group_column = Group) %>%
  dplyr::filter(Group != "blank")

## ----impute-------------------------------------------------------------------
mevastatin_ft_before_impute <- mevastatin_ft %>%
  dplyr::mutate(State = "Before imputation")
  
mevastatin_ft <- impute_knn(mevastatin_ft)

## ----viz-impute---------------------------------------------------------------
mevastatin_ft %>%
  dplyr::mutate(State = "After imputation") %>%
  dplyr::bind_rows(mevastatin_ft_before_impute) %>%
  dplyr::mutate(State = factor(State, 
                               levels = c("Before imputation", 
                                          "After imputation"))) %>%
  dplyr::group_by(State, Sample, Group) %>%
  dplyr::count(wt = is.na(Intensity)) %>%
  ggplot2::ggplot(ggplot2::aes(Sample, n, color = Group)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~State, nrow = 2) +
    ggplot2::ylab("Number of missing values") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   legend.position = "bottom")

## ----normalize----------------------------------------------------------------
mevastatin_ft_before_norm <- mevastatin_ft %>%
  dplyr::mutate(State = "Before normalization")

mevastatin_ft <- normalize_quantile_smooth(mevastatin_ft)

## ----viz-norm-----------------------------------------------------------------
mevastatin_ft %>%
  dplyr::mutate(State = "After normalization") %>%
  dplyr::bind_rows(mevastatin_ft_before_norm) %>%
  dplyr::mutate(State = factor(State, 
                               levels = c("Before normalization", 
                                          "After normalization"))) %>%
  ggplot2::ggplot(ggplot2::aes(Sample, log10(Intensity), color = Group)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~State, nrow = 2) +
    ggplot2::ylab("log10(Intensity)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   legend.position = "bottom")

## ----show-sample-metadata-----------------------------------------------------
mevastatin_metadata

## -----------------------------------------------------------------------------
mevastatin_ft <- collapse_mean(mevastatin_ft,
                               sample_metadata_cols = "Group",
                               feature_metadata_cols = "Feature")

## ----plot-volc, warning=FALSE-------------------------------------------------
plot_volcano(mevastatin_ft,
             group_column = Group,
             name_column = Feature, 
             groups_to_compare = c("control", "mevastatin")) +
  ggplot2::geom_hline(yintercept = -log10(0.05),
                      color = "grey40",
                      linetype = 2) +
  ggplot2::geom_vline(xintercept = c(- 1, 1),
                      color = "grey40",
                      linetype = 2) +
  ggplot2::theme_bw()

