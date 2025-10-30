## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(nuggets)
library(dplyr)
library(ggplot2)
library(tidyr)

options(tibble.width = Inf)

## -----------------------------------------------------------------------------
# Transform the whole dataset to crisp predicates
# First, convert cyl to a factor for illustration
crisp_mtcars <- mtcars |>
    mutate(cyl = factor(cyl, levels = c(4, 6, 8), labels = c("four", "six", "eight"))) |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "crisp", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "crisp", .breaks = 3) 

head(crisp_mtcars, n = 3)

## ----message=FALSE------------------------------------------------------------
# Start with fresh mtcars and transform to fuzzy predicates
fuzzy_mtcars <- mtcars |>
    mutate(cyl = factor(cyl, levels = c(4, 6, 8), labels = c("four", "six", "eight"))) |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "triangle", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "triangle", .breaks = 3) 

head(fuzzy_mtcars, n = 3)

## -----------------------------------------------------------------------------
disj <- var_names(colnames(fuzzy_mtcars))
print(disj)

## -----------------------------------------------------------------------------
result <- dig_associations(fuzzy_mtcars,
                           antecedent = !starts_with("am"),
                           consequent = starts_with("am"),
                           disjoint = disj,
                           min_support = 0.02,
                           min_confidence = 0.8,
                           measures = c("lift", "conviction"),
                           contingency_table = TRUE)

## -----------------------------------------------------------------------------
result <- arrange(result, desc(support))
print(result)

## -----------------------------------------------------------------------------
#head(fuzzyCO2)
#print(disj)

## -----------------------------------------------------------------------------
min_support <- 0.02
min_confidence <- 0.8

f <- function(condition, support, foci_supports) {
    conf <- foci_supports / support
    sel <- !is.na(conf) & conf >= min_confidence & !is.na(foci_supports) & foci_supports >= min_support
    conf <- conf[sel]
    supp <- foci_supports[sel]
    
    lapply(seq_along(conf), function(i) { 
      list(antecedent = format_condition(names(condition)),
           consequent = format_condition(names(conf)[[i]]),
           support = supp[[i]],
           confidence = conf[[i]])
    })
}

## -----------------------------------------------------------------------------
#result <- dig(fuzzyCO2,
              #f = f,
              #condition = !starts_with("Treatment"),
              #focus = starts_with("Treatment"),
              #disjoint = disj,
              #min_length = 1,
              #min_support = min_support)

## -----------------------------------------------------------------------------
#result <- result |>
  #unlist(recursive = FALSE) |>
  #lapply(as_tibble) |>
  #do.call(rbind, args = _) |>
  #arrange(desc(support))
#
#print(result)

