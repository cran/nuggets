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
# For demonstration, convert 'cyl' column of the mtcars dataset to a factor
mtcars <- mtcars |>
    mutate(cyl = factor(cyl, levels = c(4, 6, 8), labels = c("four", "six", "eight")))

head(mtcars, n = 3)

## -----------------------------------------------------------------------------
# Transform the whole dataset to crisp predicates
crisp_mtcars <- mtcars |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "crisp", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "crisp", .breaks = 3) 

head(crisp_mtcars, n = 3)

## ----message=FALSE------------------------------------------------------------
# Start with fresh mtcars and transform to fuzzy predicates
fuzzy_mtcars <- mtcars |>
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
                           contingency_table = TRUE)

## -----------------------------------------------------------------------------
result <- arrange(result, desc(support))
print(result)

## -----------------------------------------------------------------------------
# Prepare combined dataset with both condition predicates and numeric variables
combined_mtcars <- cbind(crisp_mtcars, mtcars[, c("mpg", "disp", "hp", "wt")])

# Extend disjoint vector for the new numeric columns
disj_combined <- c(var_names(colnames(crisp_mtcars)),
                   c("mpg", "disp", "hp", "wt"))

# Search for conditional correlations
corr_result <- dig_correlations(combined_mtcars,
                                condition = colnames(crisp_mtcars),
                                xvars = c("mpg", "hp"),
                                yvars = c("wt", "disp"),
                                disjoint = disj_combined,
                                min_length = 1,
                                max_length = 2,
                                min_support = 0.2,
                                method = "pearson")

print(corr_result)

## -----------------------------------------------------------------------------
# Prepare combined dataset with predicates and numeric variables
combined_mtcars2 <- cbind(crisp_mtcars, 
                          mtcars[, c("mpg", "hp", "wt")])

# Extend disjoint vector for the new numeric columns
disj_combined2 <- c(var_names(colnames(crisp_mtcars)),
                    c("mpg", "hp", "wt"))

# Search for baseline contrasts
baseline_result <- dig_baseline_contrasts(combined_mtcars2,
                                         condition = colnames(crisp_mtcars),
                                         vars = c("mpg", "hp", "wt"),
                                         disjoint = disj_combined2,
                                         min_length = 1,
                                         max_length = 2,
                                         min_support = 0.2,
                                         method = "t")

head(baseline_result)

## -----------------------------------------------------------------------------
complement_result <- dig_complement_contrasts(combined_mtcars2,
                                             condition = colnames(crisp_mtcars),
                                             vars = c("mpg", "hp", "wt"),
                                             disjoint = disj_combined2,
                                             min_length = 1,
                                             max_length = 2,
                                             min_support = 0.15,
                                             method = "t")

head(complement_result)

## -----------------------------------------------------------------------------
paired_result <- dig_paired_baseline_contrasts(combined_mtcars2,
                                              condition = colnames(crisp_mtcars),
                                              xvars = c("mpg", "hp"),
                                              yvars = c("wt", "wt"),
                                              disjoint = disj_combined2,
                                              min_length = 1,
                                              max_length = 2,
                                              min_support = 0.2,
                                              method = "t")

head(paired_result)

## ----fig.width=8, fig.height=5------------------------------------------------
# Search for rules with various confidence levels for visualization
vis_rules <- dig_associations(fuzzy_mtcars,
                              antecedent = starts_with(c("gear", "vs")),
                              consequent = "am=1",
                              disjoint = disj,
                              min_support = 0,
                              min_confidence = 0,
                              min_length = 0,
                              max_length = 3,
                              max_results = 50)
print(vis_rules)

# Create diamond plot showing rule hierarchy
ggplot(vis_rules) +
    aes(condition = antecedent,
        fill = confidence,
        linewidth = confidence,
        size = support,
        label = paste0(antecedent, "\nconf: ", round(confidence, 2))) +
    geom_diamond(nudge_y = 0.25) +
    scale_x_discrete(expand = expansion(add = 0.5)) +
    scale_y_discrete(expand = expansion(add = 0.25)) +
    labs(title = "Association Rules Hierarchy",
         subtitle = "consequent: am=1")

## ----eval=FALSE---------------------------------------------------------------
# # Launch interactive explorer for association rules
# rules <- dig_associations(fuzzy_mtcars,
#                          antecedent = everything(),
#                          consequent = everything(),
#                          min_support = 0.05,
#                          min_confidence = 0.7)
# 
# # Open interactive explorer
# explore(rules, data = fuzzy_mtcars)

## -----------------------------------------------------------------------------
# Define thresholds for custom association rules
min_support <- 0.02
min_confidence <- 0.8

# Define custom callback function
f <- function(condition, support, pp, pn) {
    # Calculate confidence for each focus (consequent)
    conf <- pp / support
    
    # Filter rules by confidence and support thresholds
    sel <- !is.na(conf) & conf >= min_confidence & !is.na(pp) & pp >= min_support
    conf <- conf[sel]
    supp <- pp[sel]
    
    # Return list of rules meeting criteria
    lapply(seq_along(conf), function(i) { 
      list(antecedent = format_condition(names(condition)),
           consequent = names(conf)[[i]],
           support = supp[[i]],
           confidence = conf[[i]])
    })
}

# Search using custom callback
custom_result <- dig(fuzzy_mtcars,
                    f = f,
                    condition = !starts_with("am"),
                    focus = starts_with("am"),
                    disjoint = disj,
                    min_length = 1,
                    min_support = min_support)

# Flatten and format results
custom_result <- custom_result |>
  unlist(recursive = FALSE) |>
  lapply(as_tibble) |>
  do.call(rbind, args = _) |>
  arrange(desc(support))

print(custom_result)

## -----------------------------------------------------------------------------
# Define callback for grid-based patterns
grid_callback <- function(d, weights) {
    if (nrow(d) < 5) return(NULL)  # Skip if too few observations
    
    # Compute weighted correlation
    wcor <- cov.wt(d, wt = weights, cor = TRUE)$cor[1, 2]
    
    list(
        correlation = wcor,
        n_obs = sum(weights > 0.1),
        mean_x = weighted.mean(d[[1]], weights),
        mean_y = weighted.mean(d[[2]], weights)
    )
}

# Prepare combined dataset
combined_fuzzy <- cbind(fuzzy_mtcars, mtcars[, c("mpg", "hp", "wt")])

# Extend disjoint vector for new numeric columns
combined_disj3 <- c(var_names(colnames(fuzzy_mtcars)),
                    c("mpg", "hp", "wt"))

# Search using grid approach
grid_result <- dig_grid(combined_fuzzy,
                       f = grid_callback,
                       condition = colnames(fuzzy_mtcars),
                       xvars = c("mpg", "hp"),
                       yvars = c("wt"),
                       disjoint = combined_disj3,
                       type = "fuzzy",
                       min_length = 1,
                       max_length = 2,
                       min_support = 0.15,
                       max_results = 20)

# Display results
print(grid_result)

