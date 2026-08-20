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
crisp_iris <- iris |>
    partition(Species) |>
    partition(Sepal.Length:Petal.Width, .method = "crisp", .breaks = 3)

head(crisp_iris, n = 3)

## -----------------------------------------------------------------------------
simple_callback <- function(condition) {
    str(condition)
    cat("------\n")
    
    list(condition = format_condition(names(condition)))
}

simple_result <- dig(x = crisp_iris,
                     f = simple_callback,
                     condition = starts_with("Sepal"),
                     min_length = 0,
                     max_length = 2,
                     min_support = 0.2)

## -----------------------------------------------------------------------------
str(simple_result)

## -----------------------------------------------------------------------------
bind_rows(simple_result)

## -----------------------------------------------------------------------------
attributes(simple_result)$call_args$condition

## -----------------------------------------------------------------------------
focus_callback <- function(condition, sum, pp) { 
    str(list(condition = condition,
             sum = sum,
             species = pp))
    cat("------\n")

    NULL
}

focus_result <- dig(x = crisp_iris,
                    f = focus_callback,
                    condition = starts_with("Sepal"),
                    focus = starts_with("Species"),
                    min_length = 2,
                    max_length = 2,
                    max_results = 1)

## -----------------------------------------------------------------------------
focus_callback <- function(condition, sum, pp) { 
    species_names <- names(pp)
    species_counts <- as.integer(pp)

    lapply(seq_along(species_names), function(i) {
        list(condition = format_condition(names(condition)),
             species = species_names[i],
             condition_count = sum,
             species_count = species_counts[i])
    })
}
    
focus_result <- dig(x = crisp_iris,
                    f = focus_callback,
                    condition = starts_with("Sepal"),
                    focus = starts_with("Species"),
                    min_length = 0,
                    max_length = 2) 

## -----------------------------------------------------------------------------
focus_result |>
    unlist(recursive = FALSE) |>
    bind_rows() |>
    head(n = 6)

## -----------------------------------------------------------------------------
min_support <- 0.1
min_confidence <- 0.8

rule_callback <- function(condition, pp, support) {
    conf <- pp / support / nrow(crisp_iris)
    sel <- !is.na(conf) & conf >= min_confidence & !is.na(pp) & pp >= min_support
    conf <- conf[sel]
    supp <- pp[sel] / nrow(crisp_iris)

    lapply(seq_along(conf), function(i) {
        list(antecedent = format_condition(names(condition)),
             consequent = names(conf)[[i]],
             antecedent_support = support,
             rule_support = supp[[i]],
             confidence = conf[[i]]
        )
    })
}

rule_result <- dig(x = crisp_iris,
                   f = rule_callback,
                   condition = !starts_with("Species"),
                   focus = starts_with("Species"),
                   min_length = 1,
                   min_support = min_support,
                   min_focus_support = min_support,
                   min_conditional_focus_support = min_confidence,
                   filter_empty_foci = TRUE) |>
    unlist(recursive = FALSE) |>
    bind_rows() |>
    arrange(desc(confidence))

head(rule_result, n = 6)

## -----------------------------------------------------------------------------
correlation_callback <- function(condition, support, indices) {
    if (length(indices) < 10) {
        return(NULL)
    }
    fit <- cor.test(iris$Sepal.Length[indices],
                    iris$Petal.Length[indices],
                    method = "pearson")

    list(condition = format_condition(names(condition)),
         support = support,
         correlation = unname(fit$estimate),
         p_value = fit$p.value,
         n = length(indices))
}

correlation_result <- dig(x = crisp_iris,
                          f = correlation_callback,
                          condition = everything(),
                          min_length = 1,
                          max_length = 2,
                          min_support = 0.1) |>
    bind_rows() |>
    arrange(desc(abs(correlation)))

head(correlation_result, n = 6)

## -----------------------------------------------------------------------------
fuzzy_iris <- iris |>
    partition(Species) |>
    partition(Sepal.Length:Petal.Width, .method = "triangle", .breaks = 3)

head(fuzzy_iris, n = 3)

fuzzy_callback <- function(condition, indices, weights) {
    if (length(indices) < 20) {
        return(NULL)
    }

    list(condition = format_condition(names(condition)),
         nonzero_rows = sum(indices),
         weighted_support = sum(weights) / nrow(fuzzy_iris),
         mean_petal_length_by_indices = mean(iris$Petal.Length[indices]),
         mean_petal_length_by_weights = weighted.mean(iris$Petal.Length, weights))
}

fuzzy_result <- dig(x = fuzzy_iris,
                    f = fuzzy_callback,
                    condition = starts_with("Sepal"),
                    min_length = 1,
                    max_length = 1,
                    min_support = 0.2) |>
    bind_rows()

fuzzy_result

