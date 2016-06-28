#' Plot a binary matrix optionally with tiles.
#'
#' Plot a binary matrix optionally with tiles. A bit is plotted as an
#' asterisk and a zero as a dot (or optionally not plotted).
#'
#' @param x Data matrix.
#' @param tiles A matrix with tiles.
#' @param plot_data Should the data be plotted (Boolean, default is TRUE).
#' @param plot_dot Should a dot be plotted for a zero (Boolean, default is FALSE).
#' @param main Plot title.
#'
#' @return Nothing
#'
#' @export
plot_binary_matrix <- function(x, tiles = NULL, tiles_scaled = NULL, xsignal = NULL, plot_data = TRUE, plot_discrete = TRUE, plot_signal = FALSE, plot_dot = FALSE, main = "", savename = NULL, plot_y_labels = TRUE, xlab = "", ylab = "", perf_string = TRUE, skipxlabel = 30, plot_x_labels = FALSE, xvec = NULL, pointcol = "black", cex.ylab = 1, cex.main = 2, cex.axis = 2, cex.xlab = 1) {
    if (! is.null(savename))
        pdf(savename, width = 6, height = 6)

    if (perf_string) {
        if (! is.null(tiles)) {
            ## Performance indices as plot title
            perf_string <-  perf_all_metrics(x, tiles, as_string = TRUE)

            if (main == "") {
                main <- perf_string
            } else {
                main <- paste0(main, "\n", perf_string)
            }
        }
    }

    if (! is.null(tiles_scaled)) {
        tiles <- tiles_scaled
        x <- xsignal
    }

    ## Switch the rows in x
    x <- x[seq.int(nrow(x), 1, -1), ]

    ## prepare plot
    plot(0,0, xlim = c(1, ncol(x)), ylim = c(0, nrow(x)), type = "n", xaxt = "n", yaxt = "n", xlab = xlab, ylab = ylab, main = main, cex.lab = 2, cex.main = cex.main, cex.axis = cex.axis)

    if (plot_data) {

        for (i in seq.int(nrow(x))) {
            if (plot_discrete) {
                ind <- which(x[i,] == 1)
                points(x = ind, y = rep(i, length(ind)), pch = ".", col = pointcol, bg = "black", cex = 1)

                if (plot_dot) {
                    ind <- which(x[i,] == 0)
                    points(x = ind, y = rep(i, length(ind)), pch = ".", col = "black", bg = "black", cex = 1)
                }
            }

            if (plot_signal) {
                sig <- 0.5 * (xsignal[i,] / max(abs(xsignal[i,])))
                lines(seq.int(length(sig)), sig + i)
            }
        }
    }

        if (! is.null(rownames(x))) {
            if (plot_y_labels) {
                axis(2, at = seq.int(nrow(x)), labels = FALSE)
                text(y = seq.int(nrow(x)), par("usr")[1], labels = (rownames(x)), srt = 0, pos = 2, xpd = TRUE, cex = cex.ylab)
            }
        }

        if (plot_x_labels) {
            text(x = seq.int(from = 1, to = ncol(x), by = skipxlabel), par("usr")[3]-0.3, labels = xvec[c(1, seq.int(from = 0, to = ncol(x), by = skipxlabel))], srt = 45, pos = 2, xpd = TRUE, cex = cex.xlab)

        }

    
    d <- 0.5

        if (! is.null(tiles)) {
        n_tiles  <- length(tiles)
        colorvec <- rainbow(n_tiles, alpha = 0.7)

        nr <- nrow(x) + 1

        ## rect(tiles[i,3]-d, nr-tiles[i,2]-d, tiles[i,4]+d, nr-tiles[i,2]+d, col = colorvec[tiles[i,5]], border = NA)
        for (i in seq.int(n_tiles)) {
            for (row_id in tiles[[i]]$rows) {
                rect(tiles[[i]]$colstart-d, nr-row_id-d, tiles[[i]]$colstop+d, nr-row_id+d, col = colorvec[i], border = NA)
            }
        }


        if (! is.null(savename))
            dev.off()

    }
}
