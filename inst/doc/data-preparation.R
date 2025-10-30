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
# Create a copy to avoid modifying the original dataset
mtcars_example <- mtcars
mtcars_example$cyl <- factor(mtcars_example$cyl,
                     levels= c(4, 6, 8),
                     labels = c("four", "six", "eight"))
head(mtcars_example)

## -----------------------------------------------------------------------------
partition(mtcars_example, cyl)

## -----------------------------------------------------------------------------
partition(mtcars_example, vs:gear, .method = "dummy")

## -----------------------------------------------------------------------------
partition(mtcars_example, mpg, .method = "crisp", .breaks = c(-Inf, 15, 20, 30, Inf))

## -----------------------------------------------------------------------------
partition(mtcars_example, disp, .method = "crisp", .breaks = 3)

## -----------------------------------------------------------------------------
crisp_mtcars <- mtcars_example |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "crisp", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "crisp", .breaks = 3) 

head(crisp_mtcars, n = 3)

## -----------------------------------------------------------------------------
# Equal-width intervals (default)
partition(CO2, conc, .method = "crisp", .breaks = 4, .style = "equal")

## -----------------------------------------------------------------------------
# Quantile-based intervals (equal frequency in each interval)
partition(CO2, conc, .method = "crisp", .breaks = 4, .style = "quantile")

## -----------------------------------------------------------------------------
# K-means clustering to find natural breakpoints
partition(CO2, conc, .method = "crisp", .breaks = 4, .style = "kmeans")

## -----------------------------------------------------------------------------
# Standard deviation-based intervals
partition(CO2, conc, .method = "crisp", .breaks = 4, .style = "sd")

## -----------------------------------------------------------------------------
# Use Lloyd's algorithm for k-means
partition(CO2, conc, .method = "crisp", .breaks = 4, 
          .style = "kmeans", 
          .style_params = list(algorithm = "Lloyd"))

## -----------------------------------------------------------------------------
# Use different quantile types (see ?quantile for details)
partition(CO2, conc, .method = "crisp", .breaks = 4, 
          .style = "quantile", 
          .style_params = list(type = 7))

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = c(-10, 0, 10), .labels = "triangle", .keep = TRUE) |>
    partition(x, .method = "raisedcos", .breaks = c(-10, 0, 10), .labels = "raisedcos", .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "method", values_to = "value") |>
    mutate(method = gsub("x=", "", method)) |>
    ggplot() +
        aes(x = x, y = value, color = method) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".breaks = c(-10, 0, 10)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = c(-10, -5, 0, 5, 10), .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".breaks = c(-10, -5, 0, 5, 10)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = c(-Inf, -5, 0, 5, Inf), .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".breaks = c(-Inf, -5, 0, 5, Inf)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = 4, .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".breaks = 4") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "raisedcos", .breaks = c(-Inf, -10, -5, 0, 5, 10, Inf), .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".breaks = c(-Inf, -10, -5, 0, 5, 10, Inf)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## ----message=FALSE------------------------------------------------------------
# Start with a fresh copy of mtcars
fuzzy_mtcars <- mtcars |>
    mutate(cyl = factor(cyl, levels = c(4, 6, 8), labels = c("four", "six", "eight"))) |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "triangle", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "triangle", .breaks = 3) 

head(fuzzy_mtcars, n = 3)

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = c(-10, -5, 5, 10), .span = 2, .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".span = 2, .breaks = c(-10, -5, 5, 10)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = c(-15, -10, -5, 0, 5, 10, 15), .inc = 1, .span = 2, .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".inc = 1, .span = 2, .breaks = c(-15, -10, -5, 0, 5, 10, 15)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
data.frame(x = seq(-15, 15, length.out = 1000)) |>
    partition(x, .method = "triangle", .breaks = c(-15, -10, -5, 0, 5, 10, 15), .inc = 3, .span = 2, .keep = TRUE) |>
    pivot_longer(starts_with("x="), names_to = "fuzzy set", values_to = "value") |>
    ggplot() +
        aes(x = x, y = value, color = `fuzzy set`) +
        geom_line(size = 1.2) +
        labs(x = "x", y = "membership degree", title = ".inc = 3, .span = 2, .breaks = c(-15, -10, -5, 0, 5, 10, 15)") +
        theme_gray(base_size = 16) +
        theme(legend.position = "right")

## -----------------------------------------------------------------------------
# Completely constant vector
is_almost_constant(c(1, 1, 1, 1, 1))

# Variable vector
is_almost_constant(c(1, 2, 3, 4, 5))

# Almost constant (80% are the same value)
is_almost_constant(c(1, 1, 1, 1, 2), threshold = 0.8)

# Not almost constant with threshold 0.8
is_almost_constant(c(1, 1, 1, 2, 2), threshold = 0.8)

## -----------------------------------------------------------------------------
# With NA values - by default NA is treated as a regular value
is_almost_constant(c(NA, NA, NA, 1, 2), threshold = 0.5)

# With NA removed before computing proportions
is_almost_constant(c(NA, NA, NA, 1, 2), threshold = 0.5, na_rm = TRUE)

## -----------------------------------------------------------------------------
# Create a data frame with some constant and variable columns
d <- data.frame(
  a1 = 1:10,              # variable
  a2 = c(1:9, NA),        # variable
  b1 = "b",               # constant
  b2 = NA,                # constant (all NA)
  c1 = rep(c(TRUE, FALSE), 5),  # variable
  c2 = rep(c(TRUE, NA), 5),     # 50% TRUE, 50% NA
  d  = c(rep(TRUE, 4), rep(FALSE, 4), NA, NA)  # 40% TRUE, 40% FALSE, 20% NA
)

# Remove columns that are completely constant
remove_almost_constant(d, .threshold = 1.0, .na_rm = FALSE)

# Remove columns where the majority value occurs in >= 50% of rows
remove_almost_constant(d, .threshold = 0.5, .na_rm = FALSE)

# Same as above, but removing NA before computing proportions
remove_almost_constant(d, .threshold = 0.5, .na_rm = TRUE)

## -----------------------------------------------------------------------------
# Only check columns a1 through b2
remove_almost_constant(d, a1:b2, .threshold = 0.5, .na_rm = TRUE)

## -----------------------------------------------------------------------------
# Prepare mtcars data with partition - use fresh copy
prepared_data <- mtcars |>
    mutate(cyl = factor(cyl, levels = c(4, 6, 8), labels = c("four", "six", "eight"))) |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg:carb, .method = "crisp", .breaks = 3)

# Check for and remove any almost constant columns
prepared_data <- remove_almost_constant(prepared_data, 
                                       .threshold = 0.95, 
                                       .verbose = TRUE)

## -----------------------------------------------------------------------------
# Prepare fuzzy data - use fresh copy of mtcars
fuzzy_mtcars <- mtcars |>
    mutate(cyl = factor(cyl, levels = c(4, 6, 8), labels = c("four", "six", "eight"))) |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg:carb, .method = "triangle", .breaks = 3)

# Create disjoint vector
disj <- var_names(colnames(fuzzy_mtcars))

# Find tautologies with very high confidence
tautologies <- dig_tautologies(
    fuzzy_mtcars,
    antecedent = everything(),
    consequent = everything(),
    disjoint = disj,
    min_confidence = 0.95,
    min_support = 0.1,
    max_length = 3,
    t_norm = "goguen"
)

print(tautologies)

## ----eval=FALSE---------------------------------------------------------------
# # Convert tautologies to excluded format
# excluded_conditions <- parse_condition(tautologies$antecedent)
# 
# # Use in subsequent pattern search
# results <- dig_associations(
#     fuzzy_mtcars,
#     antecedent = !starts_with("am"),
#     consequent = starts_with("am"),
#     disjoint = disj,
#     excluded = excluded_conditions,  # Exclude tautological patterns
#     min_support = 0.1,
#     min_confidence = 0.8
# )

