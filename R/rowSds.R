Sds <- function(...) {
    stats::sd(c(...), na.rm = T)
}

MapSds <- function(...) {
    mapply(Sds, ...)
}

#' Compute the standard deviations of the rows of a data frame.
#'
#' @param l A numeric data frame (or a list with equally long elements)
#' @return A list with the row-wise standard deviations.
#' @export
rowSds <- function(l) {
    do.call("MapSds", l)
}
