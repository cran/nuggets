## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
options(tibble.width = Inf)

## ----message=FALSE------------------------------------------------------------
library(nuggets)
library(dplyr)    # for data manipulation

## -----------------------------------------------------------------------------
head(CO2)

## -----------------------------------------------------------------------------
crisp_co2 <- CO2 |>
    select(-Plant) |>
    partition(Type, Treatment) |>
    partition(conc, .method = "crisp", .breaks = c(-Inf, 200, 500, Inf)) |>
    partition(uptake, .method = "crisp", .breaks = c(-Inf, 20, 35, Inf))

head(crisp_co2, n = 3)

## -----------------------------------------------------------------------------
fuzzy_co2 <- CO2 |>
    select(-Plant) |>
    partition(Type, Treatment) |>
    partition(conc, .method = "triangle", .breaks = c(-Inf, 95, 500, 1000, Inf)) |>
    partition(uptake, .method = "triangle", .breaks = c(-Inf, 10, 25, 40, Inf))

head(fuzzy_co2, n = 3)

## -----------------------------------------------------------------------------
rules <- dig_associations(crisp_co2,
                          min_support = 0.1,
                          min_confidence = 0.8)
head(rules)

## ----include=FALSE------------------------------------------------------------
to_logical <- function(x) {
    x <- gsub("{", "", x, fixed = TRUE)
    x <- gsub("}", "", x, fixed = TRUE)
    x <- gsub(",", " & ", x, fixed = TRUE)
}

## -----------------------------------------------------------------------------
rules_uptake <- dig_associations(crisp_co2,
                                 antecedent = !starts_with("uptake"),
                                 consequent = starts_with("uptake"),
                                 min_support = 0.1,
                                 min_confidence = 0.8)
head(rules_uptake, n = 3)

## -----------------------------------------------------------------------------
disj <- var_names(colnames(crisp_co2))
disj

rules <- dig_associations(crisp_co2,
                          disjoint = disj,
                          min_support = 0.1,
                          min_confidence = 0.8)
head(rules, n = 3)

## -----------------------------------------------------------------------------
# Find only rules with exactly 2 predicates in the antecedent
rules <- dig_associations(crisp_co2,
                          min_length = 2,
                          max_length = 2,
                          min_support = 0.1,
                          min_confidence = 0.8)
head(rules, n = 3)

## -----------------------------------------------------------------------------
rules <- dig_associations(crisp_co2,
                          min_support = 0.05,
                          min_confidence = 0.6,
                          max_results = 5)
nrow(rules)

## -----------------------------------------------------------------------------
# Fuzzy rules using the product t-norm (default)
fuzzy_rules <- dig_associations(fuzzy_co2,
                                antecedent = !starts_with("uptake"),
                                consequent = starts_with("uptake"),
                                min_support = 0.05,
                                min_confidence = 0.6,
                                t_norm = "goguen")
head(fuzzy_rules, n = 3)

## -----------------------------------------------------------------------------
# Add selected interest measures
rules_enriched <- rules_uptake |>
    add_interest(measures = c("conviction", "leverage", "jaccard"))
rules_enriched |>
    select(antecedent, consequent, confidence, conviction, leverage, jaccard) |>
    head(n = 3)

## -----------------------------------------------------------------------------
rules_all_measures <- rules_uptake |>
    add_interest()
colnames(rules_all_measures)

## ----eval=FALSE---------------------------------------------------------------
# ?add_interest

## -----------------------------------------------------------------------------
rules_smoothed <- rules_uptake |>
    add_interest(measures = c("odds_ratio", "conviction"),
                 smooth_counts = 0.5)
rules_smoothed |>
    select(antecedent, consequent, confidence, odds_ratio, conviction) |>
    head(n = 3)

## -----------------------------------------------------------------------------
rules_guha <- rules_uptake |>
    add_interest(measures = c("dfi", "fe", "lci"),
                 p = 0.5)
rules_guha |>
    select(antecedent, consequent, confidence, dfi, fe, lci) |>
    head(n = 3)

## -----------------------------------------------------------------------------
tautologies <- dig_tautologies(crisp_co2,
                               antecedent = everything(),
                               consequent = everything(),
                               min_confidence = 0.95,
                               min_support = 0.05,
                               max_length = 2)
tautologies

## -----------------------------------------------------------------------------
# Convert tautologies to the excluded (axioms) format
excluded_conds <- parse_condition(tautologies$antecedent,
                                  tautologies$consequent)

# Search for rules while excluding entailed patterns
rules_filtered <- dig_associations(crisp_co2,
                                   antecedent = !starts_with("uptake"),
                                   consequent = starts_with("uptake"),
                                   excluded = excluded_conds,
                                   min_support = 0.1,
                                   min_confidence = 0.8)
rules_filtered

## -----------------------------------------------------------------------------
# Axiom: "Treatment=chilled => Type=Mississippi"
# Any rule whose consequent is "Type=Mississippi" and whose antecedent contains
# "Treatment=chilled" will be excluded, because the consequent is deducible
# from the antecedent via this axiom.
manual_excluded <- list(c("Treatment=chilled", "Type=Mississippi"))

rules_manual <- dig_associations(crisp_co2,
                                 antecedent = !starts_with("uptake"),
                                 consequent = starts_with("uptake"),
                                 excluded = manual_excluded,
                                 min_support = 0.1,
                                 min_confidence = 0.8)
rules_manual

