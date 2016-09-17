source("main.R")

scale_tiles <- function(tiles) {
    for (i in seq.int(length(tiles))) {
        tiles[[i]]$colstart <- tiles[[i]]$colstart * 30
        tiles[[i]]$colstop  <- tiles[[i]]$colstop * 30
    }
    tiles
}


make_result_figure <- function(X, K, alpha, timevec, figname) {
    ## Perform tiling
    tiles_mt <- max_tile(X = X, K = K, alpha = alpha)
    tiles_gt <- global_tile(X = X, K = K, alpha = alpha)

    ## Map tiles back to the original time scale
    tiles_mts <- scale_tiles(tiles_mt)
    tiles_gts <- scale_tiles(tiles_gt)

    ## Plot the figure
    pdf(file = figname, width = 9, height = 7)
    par(mfcol = c(1,2))
    plot_binary_matrix(X, xsignal = XS, tiles = tiles_mt, tiles_scaled = tiles_mts, main = "MaxTile", xlab = "", plot_discrete = FALSE, plot_signal = TRUE, plot_x_labels = TRUE, xvec = timevec, skipxlabel = 200, cex.ylab = 2, cex.main = 2, cex.axis= 0.7, cex.xlab = 2)
    plot_binary_matrix(X, xsignal = XS, tiles = tiles_gt, tiles_scaled = tiles_gts, main = "GlobalTile", xlab = "", plot_discrete = FALSE, plot_signal = TRUE, plot_x_labels = TRUE, xvec = timevec, skipxlabel = 200, cex.ylab = 2, cex.main = 2, cex.axis= 0.7, cex.xlab = 2)
    dev.off()
}

data_real <- make_dataset_list_real()

XR <- read_data_stock_indices()
XS <- discretize_data(XR, type = "scale")

timevec <- read_data_stock_indices(return_date = TRUE)$date

set.seed(42)

economic_growth  <- TRUE
economic_decline <- TRUE
economic_stable  <- TRUE

## Set parameters
if (economic_decline) {
    X  <- data_real[["stockindicesdown"]]
    K <- 5
    alpha <- 0.85
    figname <- "results/motivating_example_stock_indices_down.pdf"

    make_result_figure(X, K, alpha, timevec, figname)
}

if (economic_growth) {
    X  <- data_real[["stockindicesup"]]
    K <- 5
    alpha <- 0.85
    figname <- "results/motivating_example_stock_indices_up.pdf"

    make_result_figure(X, K, alpha, timevec, figname)
}

if (economic_stable) {
    X  <- data_real[["stockindicesstable"]]
    K <- 5
    alpha <- 0.85
    figname <- "results/motivating_example_stock_indices_stable.pdf"

    make_result_figure(X, K, alpha, timevec, figname)
}
