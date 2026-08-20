## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    echo = FALSE,
    comment = "#>",
    fig.width = 7,
    fig.height = 4
)
options(tibble.width = Inf)

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(kableExtra)

preprocess <- function(d) {
    d |>
        pivot_longer(cols = c("nuggets", "arules_apriori", "arules_eclat"),
                     names_to = "method",
                     values_to = "time")  |>
        mutate(time = time / 1e6,  # convert nanoseconds to milli-seconds
               method = dplyr::recode(method,
                                "nuggets" = "nuggets",
                                "arules_apriori" = "apriori (arules)",
                                "arules_eclat" = "eclat (arules)"),
               method = factor(method)) 
}


two_fig <- function(d, x, xname, title) {
    nice_colnames <- c(cols = "# of data columns",
                        rows = "# of data rows",
                        min_support = "min supp",
                        min_confidence = "min conf",
                        max_length = "max ante length")
    fixed <- list()
    for (col in c("rows", "cols", "min_support", "min_confidence", "max_length")) {
        if (length(unique(d[[col]])) == 1) {
            fixed[[col]] <- unique(d[[col]])
        }
    }
    subtitle <- paste0("(",
                       paste(nice_colnames[names(fixed)], fixed, sep = " = ", collapse = ", "),
                       ")")
    
    p1 <- ggplot(d) +
        aes(x = !!x, y = time, color = method) +
        geom_point() +
        geom_line() +
        labs(title = "linear scales",
             x = xname,
             y = "Time [ms]",
             color = "Method")
    
    p2 <- ggplot(d) +
        aes(x = !!x, y = time, color = method) +
        geom_point() +
        geom_line() +
        scale_x_log10() +
        scale_y_log10() +
        labs(title = "log scales",
             x = xname,
             y = "Time [ms]",
             color = "Method")
    
    p1 + p2 +
        plot_annotation(title = title, subtitle = subtitle) +
        plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom") 
    
}


tab <- function(d, title) {
    d |>
        select(rows, cols, method, time) |>
        mutate(time = round(time, 0)) |>
        pivot_wider(names_from = method, values_from = time) |>
        relocate(`eclat (arules)`, .after = `nuggets`) |>
        arrange(rows, cols) |> 
        #kable(caption = title) |> 
        kable() |> 
        kable_styling(full_width = TRUE) |> 
        add_header_above(c(" " = 1, " " = 1, "Time [ms]" = 3))
}

data <- readRDS("comparison-with-arules.rds")

dense_rows <- data$dense_rows |> preprocess()
dense_cols <- data$dense_cols |> preprocess()
sparse_rows <- data$sparse_rows |> preprocess()
sparse_cols <- data$sparse_cols |> preprocess()

## -----------------------------------------------------------------------------
tab(dense_rows, "Execution time on dense data with varying number of rows")

## -----------------------------------------------------------------------------
two_fig(dense_rows, 
        sym("rows"),
        "Number of data rows",
        "Execution time on dense data with varying number of rows")

## -----------------------------------------------------------------------------
tab(dense_cols, "Execution time on dense data with varying number of columns")

## -----------------------------------------------------------------------------
two_fig(dense_cols, 
        sym("cols"),
        "Number of data columns",
        "Execution time on dense data with varying number of columns")

## -----------------------------------------------------------------------------
tab(sparse_rows, "Execution time on sparse data with varying number of rows")

## -----------------------------------------------------------------------------
two_fig(sparse_rows, 
        sym("rows"),
        "Number of data rows",
        "Execution time on sparse data with varying number of rows")

## -----------------------------------------------------------------------------
tab(sparse_cols, "Execution time on sparse data with varying number of columns")

## -----------------------------------------------------------------------------
two_fig(sparse_cols, 
        sym("cols"),
        "Number of data columns",
        "Execution time on sparse data with varying number of columns")

