## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(nuggets)
library(dplyr)

## -----------------------------------------------------------------------------
head(CO2)

## -----------------------------------------------------------------------------
partition(CO2, Plant:Treatment)

## -----------------------------------------------------------------------------
partition(CO2, conc, .method = "crisp", .breaks = c(-Inf, 175, 350, 675, Inf))

## -----------------------------------------------------------------------------
crispCO2 <- CO2 |>
    partition(Plant:Treatment) |>
    partition(conc, .method = "crisp", .breaks = c(-Inf, 175, 350, 675, Inf)) |>
    partition(uptake, .method = "crisp", .breaks = c(-Inf, 10, 20, Inf))

head(crispCO2)

## -----------------------------------------------------------------------------
colnames(crispCO2)

## ----message=FALSE------------------------------------------------------------
fuzzyCO2 <- CO2 |>
    partition(Plant:Treatment) |>
    partition(conc, .method = "triangle", .breaks = c(-Inf, 175, 350, 675, Inf)) |>
    partition(uptake, .method = "triangle", .breaks = c(-Inf, 18, 28, 37, Inf))

head(fuzzyCO2)
colnames(fuzzyCO2)

## -----------------------------------------------------------------------------
disj <- var_names(colnames(fuzzyCO2))
print(disj)

## -----------------------------------------------------------------------------
result <- dig_associations(fuzzyCO2,
                           antecedent = !starts_with("Treatment"),
                           consequent = starts_with("Treatment"),
                           disjoint = disj,
                           min_support = 0.02,
                           min_confidence = 0.8)

## -----------------------------------------------------------------------------
result <- arrange(result, desc(support))
print(result)

## -----------------------------------------------------------------------------
head(fuzzyCO2)
print(disj)

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
result <- dig(fuzzyCO2,
              f = f,
              condition = !starts_with("Treatment"),
              focus = starts_with("Treatment"),
              disjoint = disj,
              min_length = 1,
              min_support = min_support)

## -----------------------------------------------------------------------------
result <- result |>
  unlist(recursive = FALSE) |>
  lapply(as_tibble) |>
  do.call(rbind, args = _) |>
  arrange(desc(support))

print(result)

