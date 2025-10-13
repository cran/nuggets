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
mtcars$cyl <- factor(mtcars$cyl,
                     levels= c(4, 6, 8),
                     labels = c("four", "six", "eight"))
head(mtcars)

## -----------------------------------------------------------------------------
partition(mtcars, cyl)

## -----------------------------------------------------------------------------
partition(mtcars, vs:gear, .method = "dummy")

## -----------------------------------------------------------------------------
partition(mtcars, mpg, .method = "crisp", .breaks = c(-Inf, 15, 20, 30, Inf))

## -----------------------------------------------------------------------------
partition(mtcars, disp, .method = "crisp", .breaks = 3)

## -----------------------------------------------------------------------------
crispMtcars <- mtcars |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "crisp", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "crisp", .breaks = 3) 

head(crispMtcars, n = 3)

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
fuzzyMtcars <- mtcars |>
    partition(cyl, vs:gear, .method = "dummy") |>
    partition(mpg, .method = "triangle", .breaks = c(-Inf, 15, 20, 30, Inf)) |>
    partition(disp:carb, .method = "triangle", .breaks = 3) 

head(fuzzyMtcars, n = 3)

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
#disj <- var_names(colnames(fuzzyCO2))
#print(disj)

## -----------------------------------------------------------------------------
#result <- dig_associations(fuzzyCO2,
                           #antecedent = !starts_with("Treatment"),
                           #consequent = starts_with("Treatment"),
                           #disjoint = disj,
                           #min_support = 0.02,
                           #min_confidence = 0.8)

## -----------------------------------------------------------------------------
#result <- arrange(result, desc(support))
#print(result)

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

