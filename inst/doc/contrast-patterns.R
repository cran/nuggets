## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
options(tibble.width = Inf)

## ----message = FALSE----------------------------------------------------------
library(nuggets)
library(dplyr)     # for data manipulation

## -----------------------------------------------------------------------------
iris_contrasts <- iris |>
    mutate(long_sepal = Sepal.Length >= median(Sepal.Length),
           wide_petal = Petal.Width >= median(Petal.Width),
           length_gap = Sepal.Length - Petal.Length,
           width_gap = Sepal.Width - Petal.Width,
           sepal_ratio = Sepal.Length / Sepal.Width,
           petal_ratio = Petal.Length / Petal.Width) |>
    partition(Species)

head(iris_contrasts, n = 3)

## -----------------------------------------------------------------------------
baseline_result <- dig_baseline_contrasts(iris_contrasts,
                                          condition = where(is.logical),
                                          vars = c(length_gap, width_gap),
                                          min_length = 1,
                                          max_length = 2,
                                          min_support = 0.2,
                                          method = "t",
                                          max_p_value = 0.01)

head(baseline_result, n = 6)

## -----------------------------------------------------------------------------
baseline_wilcox <- dig_baseline_contrasts(iris_contrasts,
                                          condition = starts_with("Species"),
                                          vars = length_gap,
                                          min_length = 1,
                                          max_length = 1,
                                          min_support = 0.2,
                                          method = "wilcox",
                                          max_p_value = 0.01)

baseline_wilcox

## -----------------------------------------------------------------------------
complement_result <- dig_complement_contrasts(iris_contrasts,
                                              condition = where(is.logical),
                                              vars = c(Sepal.Length, Petal.Length, petal_ratio),
                                              min_length = 1,
                                              max_length = 2,
                                              min_support = 0.2,
                                              method = "t",
                                              max_p_value = 0.01)

head(complement_result, n = 6)

## -----------------------------------------------------------------------------
complement_var <- dig_complement_contrasts(iris_contrasts,
                                           condition = starts_with("Species"),
                                           vars = Petal.Length,
                                           min_length = 1,
                                           max_length = 1,
                                           min_support = 0.2,
                                           method = "var",
                                           max_p_value = 0.01)

complement_var

## -----------------------------------------------------------------------------
paired_result <- dig_paired_baseline_contrasts(iris_contrasts,
                                               condition = where(is.logical),
                                               xvars = c(Sepal.Length, Sepal.Width),
                                               yvars = c(Petal.Length, Petal.Width),
                                               min_length = 1,
                                               max_length = 1,
                                               min_support = 0.2,
                                               method = "t",
                                               max_p_value = 0.01)

head(paired_result, n = 6)

## -----------------------------------------------------------------------------
paired_wilcox <- dig_paired_baseline_contrasts(iris_contrasts,
                                               condition = starts_with("Species"),
                                               xvars = Sepal.Length,
                                               yvars = Petal.Length,
                                               min_length = 1,
                                               max_length = 1,
                                               min_support = 0.2,
                                               method = "wilcox",
                                               max_p_value = 0.01)

paired_wilcox

## -----------------------------------------------------------------------------
complement_result$p_holm <- p.adjust(complement_result$p_value, method = "holm")
complement_result$p_bh   <- p.adjust(complement_result$p_value, method = "BH")

complement_result[, c("condition", "var", "p_value", "p_holm", "p_bh")]

## -----------------------------------------------------------------------------
complement_result[complement_result$p_bh < 0.05, ]

## ----eval = FALSE-------------------------------------------------------------
# explore(complement_result, iris_contrasts)

