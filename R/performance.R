#' Calculate precision.
#'
#' Calculate precision (fraction of ones contained in the tiles).
#'
#' @param tiles A tiling.
#' @param x A data matrix.
#'
#' @return The precision.
#'
#' @export
perf_precision <- function(tiles, x) {
    n_1 <- 0
    n_t <- 0

    for (tile in tiles) {
        n_1 <- n_1 + sum(x[tile$rows, tile$colstart:tile$colstop])
        n_t <- n_t + length(tile$rows) * (tile$colstop - tile$colstart + 1)
    }

    n_1 / n_t
}


#' Calculate recall.
#'
#' Calculate recall (the fraction of ones in a binary matrix covered by a tiling).
#'
#' @param tiles A tiling.
#' @param x A data matrix.
#'
#' @return The recall.
#'
#' @export
perf_recall <- function(tiles, x, xsum = sum(x)) {
    sum(x[tiles2ind(tiles, dim(x))] > 0) / xsum
}


#' Calculate cardinality.
#'
#' Calculate the cardinality as the number of unique tiles (clusters) in the tiling.
#'
#' @param tiles A tiling.
#' @param x A data matrix (not used).
#'
#' @return The cardinality (integer).
#'
#' @export
perf_cardinality <- function(tiles, x) {
    length(tiles)
}


#' Calculate all performance metrics.
#'
#' Calculate all performance metrics.
#'
#' @param x A data matrix.
#' @param tiles A tiling.
#'
#' @return A list with all of the performance metrics
#'
#' @export
perf_all_metrics <- function(x, tiles, print = FALSE, as_string = FALSE, short_names = TRUE) {
    out <- list("cardinality" = perf_cardinality(tiles, x),
                "precision"   = perf_precision(tiles, x),
                "recall"      = perf_recall(tiles, x))

    if (print) {
        cat("Cardinality:\t", sprintf("%i", out[["cardinality"]]), "\n")
        cat("Precision:\t", sprintf("%.2f", out[["precision"]]), "\n")
        cat("Recall:\t\t", sprintf("%.2f", out[["recall"]]), "\n\n")
    }

    c_str <- "Cardinality"
    p_str <- "Precision "
    r_str <- "Recall"

    if (short_names) {
        c_str <- "k"
        p_str <- substr(p_str, 1, 1)
        r_str <- substr(r_str, 1, 1)
    }

    if (as_string)
        out <- paste0(c_str, ": ", sprintf("%i", out[["cardinality"]]),
                      "     ", p_str, ": ", sprintf("%.2f", out[["precision"]]),
                      "     ", r_str, ": ", sprintf("%.2f", out[["recall"]]))

    out
}
