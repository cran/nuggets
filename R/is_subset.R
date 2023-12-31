#' Determine whether the first vector is a subset of the second vector
#'
#' @param x the first vector
#' @param y the second vector
#' @return TRUE if `x` is a subset of `y` or FALSE otherwise.
#' @author Michal Burda
#' @export
is_subset <- function(x, y) {
    .must_be_atomic_vector(x)
    .must_be_atomic_vector(y)

    length(setdiff(x, y)) == 0L
}
