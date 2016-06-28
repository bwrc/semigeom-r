#' Make a tile
#'
#' @param rows The rows in the tile
#' @param colstart The starting column of the tile
#' @param colstop The ending column o fthe tile
#' @param tile_id The ID of the tile. Defaults to zero.
#'
#' @return A one-row matrix with the information needed 
#'
#' @export
make_tile <- function(rows, colstart, colstop, tile_id = 0) {
    out <- matrix(0, nrow = length(rows), ncol = 5)
    out[,2] <- rows
    out[,3] <- colstart
    out[,4] <- colstop
    out[,5] <- tile_id

    colnames(out) <- c("row_id", "ts_id", "start", "stop", "cluster_id")

    out
}

#' Convert a matrix with segments to a list
#'
#' @param segmat A matrix with segments
#'
#' @return A list containing the tiles
#'
#' @export
segmat_to_list <- function(segmat) {
    n_tiles <- length(unique(segmat[,5]))
    tiles <- vector(mode = "list", length = n_tiles)
    for (i in unique(segmat[,5])) {
        ind <- which(segmat[,5] == i)
        tiles[[i]] <- list("rows" = segmat[ind,2], "colstart" = segmat[ind[1], 3], "colstop" = segmat[ind[1], 4])
    }
    tiles
}


#' Subscript to linear index
#'
#' @param row Row index
#' @param col Column indexd
#' @param nrows The number of rows in the data matrix
#'
#' @return The linear index corresponding to the subscript index (row, col)
#'
#' @export
sub2ind <- function(row, col, nrows) {
    (col - 1) * nrows + row
}


#' Linear index to subscript
#'
#' @param ind Linear index
#' @param nrows The number of rows in the data matrix
#'
#' @return The subscript index (row, col) corresponding to the linear index ind
#'
#' @export
ind2sub <- function(ind, nrows) {
    c(((ind-1) %% nrows) + 1, floor((ind-1) / nrows) + 1)
}


#' Convert a tile to linear matrix indices
#'
#' @param tile The tile
#' @param m_dim The dimensions of the data matrix
#'
#' @return Linear indices corresponding to the tile
#'
#' @export
tile2ind <- function(tile, m_dim) {
    unlist(lapply(tile$rows, function(row) seq.int(from = sub2ind(row, tile$colstart, m_dim[1]),
                                                   to = sub2ind(row, tile$colstop, m_dim[1]),
                                                   by = m_dim[1])))
}

#' Convert list of tiles to linear matrix indices
#'
#' @param tiles List of tiles
#' @param m_dim The dimensions of the data matrix
#'
#' @return List with linear indices corresponding to the tiles
#'
#' @export
tiles2ind <- function(tiles, m_dim) {
    unique(unlist(lapply(tiles, function(tile) tile2ind(tile, m_dim))))
}

#' Get tiling functions.
#'
#' Get the MaxTile and GlobalTile functions as a list.
#'
#' @param X The data matrix.
#' @param K The maximum number of tiles. Default is 1.
#' @param alpha The precision threshold. Default is 0.75.
#'
#' @return All tiles with a precision of at least alpha.
#'
#' @export
get_function_list <- function() {
    function_list <- list()

    function_list[["MaxTile"]] <- max_tile
    function_list[["GlobalTile"]] <- global_tile

    function_list
}
