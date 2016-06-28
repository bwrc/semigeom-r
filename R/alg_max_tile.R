#' Find tiles using the MaxTile algorithm.
#'
#' This function finds tiles using the MaxTile algorithm.
#'
#' @param X The data matrix.
#' @param K The maximum number of tiles. Default is 1.
#' @param alpha The precision threshold. Default is 0.75.
#'
#' @return All tiles with a precision of at least alpha.
#'
#' @export
max_tile <- function(X, K = 1, alpha = 0.75) {
    t0 <- Sys.time()
    out <- max_tile_main(X, K, alpha)
    cat("[MaxTile] Duration:", difftime(Sys.time(), t0, units = "secs"), " seconds.\n\n")
    out
}

#' Find maximum tiles given precision threshold alpha.
#'
#' This function finds all maximum tiles in the data matrix x having a precision of at least alpha.
#'
#' @param x The data matrix.
#' @param alpha The precision threshold
#' @param rowwise Proceed rowwise (boolean).
#' @param mem Memoise (boolean).
#'
#' @return All tiles with a precision of at least alpha.
#'
#' @export
max_tile_main <- function(X, K = 1, alpha = 0.75, rowwise = FALSE, mem = FALSE) {
    ## calculate sum of the input matrix needed to calculate recall
    xsum <- sum(X)

    if (mem)
        require(memoise)

    csX <- function(x) { cbind(0, t(apply(x, 1, cumsum))) }
    cs0 <- csX(X)

    if(rowwise) {
        rows0 <- function(a, b) { cs0[, b] - cs0[, a] >= alpha * (b - a) }
    } else {
        rows0 <- function(a, b) {
            x <- cs0[,b] - cs0[, a]
            p <- order(x, decreasing = TRUE)
            (cumsum(x[p]) >= alpha * (b - a) * seq.int(length(x)))[order(p)]
        }
    }

    rows <- if(mem) memoise(rows0) else rows0
    n <- dim(X)[1]
    m <- dim(X)[2]
    tt <- matrix(NA, m*(m+1)/2, 3)
    i <- 1
    best <- 0
    cs00 <- apply(cs0, 2, sum)

    for(ab in m:1) {
        for(a in 1:(m-ab+1)) {
            b <- a+ab
            if(best <= cs00[b] - cs00[a]) {
                tt[i,] <- c(a, b, sum((cs0[, b] - cs0[, a])[rows(a,b)]))
                if(best<tt[i, 3]) { best <- tt[i, 3] }
            } else {
                tt[i,] <- c(a, b, cs00[b] - cs00[a])
            }
            i <- i+1
        }
    }

    res <- matrix(NA, K, 2)
    cur_r <- 0

    for(i in 1:K) {
        tt <- tt[order(tt[, 3], decreasing = TRUE), ]
        res[i,] <- tt[1,1:2]

        ## update current recall for early stopping
        cur_r <- cur_r + sum(X[rows(res[i, 1], res[i, 2]), (res[i, 1]):(res[i, 2]-1)]) / xsum

        if (cur_r >= 1)
            break

        if(i < K) {
            X[rows(tt[1,1], tt[1,2]), tt[1,1]:(tt[1,2]-1)] <- 0
            cs <- csX(X)

            tt[1,3] <- best <- 0
            j <- 2
            while(j<=dim(tt)[1] && best <= tt[j, 3]) {
                tt[j,3] <- sum((cs[, tt[j, 2]] - cs[, tt[j, 1]])[rows(tt[j, 1], tt[j, 2])])

                if(tt[j,3] > best) { best <- tt[j, 3] }
                j <- j+1
            }
        }
    }


    if (i < K)
        res <- res[1:i,,drop=FALSE]

    apply(res, 1, function(x) list(colstart = x[1],
                                   colstop = x[2] - 1,
                                   rows = which(rows(x[1], x[2])),
                                   id = 1))
}



