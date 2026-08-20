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
iris_corr <- iris |>
    mutate(long_sepal = Sepal.Length >= median(Sepal.Length),
           wide_petal = Petal.Width >= median(Petal.Width),
           sepal_ratio = Sepal.Length / Sepal.Width,
           petal_ratio = Petal.Length / Petal.Width) |>
    partition(Species)

head(iris_corr, n = 3)

## -----------------------------------------------------------------------------
corr_basic <- dig_correlations(iris_corr,
                               condition = where(is.logical),
                               xvars = c(Sepal.Length, Sepal.Width, sepal_ratio),
                               yvars = c(Petal.Length, Petal.Width, petal_ratio),
                               min_length = 0,
                               max_length = 2,
                               min_support = 0.2)

corr_basic |>
    arrange(desc(abs(estimate))) |>
    head(n = 6)

## -----------------------------------------------------------------------------
corr_species <- dig_correlations(iris_corr,
                                 condition = starts_with("Species"),
                                 xvars = starts_with("Sepal"),
                                 yvars = starts_with("Petal"),
                                 min_length = 1,
                                 max_length = 1,
                                 min_support = 0.3)

head(corr_species, n = 6)

## -----------------------------------------------------------------------------
corr_spearman <- dig_correlations(iris_corr,
                                  condition = where(is.logical),
                                  xvars = c(Sepal.Length, Sepal.Width),
                                  yvars = c(Petal.Length, Petal.Width),
                                  method = "spearman",
                                  exact = FALSE,
                                  min_length = 1,
                                  max_length = 1,
                                  min_support = 0.2)

head(corr_spearman, n = 6)

## -----------------------------------------------------------------------------
corr_whole <- dig_correlations(
    iris_corr,
    condition = NULL,
    xvars = starts_with("Sepal"),
    yvars = starts_with("Petal")
)

corr_whole

## -----------------------------------------------------------------------------
corr_basic$p_holm <- p.adjust(corr_basic$p_value, method = "holm")
corr_basic$p_bh   <- p.adjust(corr_basic$p_value, method = "BH")

corr_basic[, c("condition", "xvar", "yvar", "p_value", "p_holm", "p_bh")]

## -----------------------------------------------------------------------------
corr_basic[corr_basic$p_bh < 0.05, ]

## ----eval = FALSE-------------------------------------------------------------
# explore(corr_basic, iris_corr)

