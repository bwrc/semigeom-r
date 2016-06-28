#' Find tiles using the GlobalTile algorithm.
#'
#' This function finds tiles using the GlobalTile algorithm.
#'
#' @param X The data matrix.
#' @param K The maximum number of tiles. Default is 1.
#' @param alpha The precision threshold. Default is 0.75.
#'
#' @return All tiles with a precision of at least alpha.
#'
#' @export
global_tile <- function(X, K = 1, alpha = 0.75) {
    t0 <- Sys.time()
    tiles <- find_global_tiles(X, num_tiles = K, precision_lowerbound = alpha)
    out <- segmat_to_list(tiles)
    cat("[GlobalTile] Duration:", difftime(Sys.time(), t0, units = "secs"), " seconds.\n\n")
    out
}

#' Segmentation using dynamic programming
#'
#' Segmentation using dynamic programming
#'
#' @param get_tile Function used to get tile.
#' @param m The width of the matrix A, or more generally, the rightmost index where the segmentation can end.
#' @param K is the number of segments.
#'
#' @return Something
#'
#' @export
dp_segment <- cmpfun(function( get_tile, m, K ) {
    ## The S matrix is a cache of segment-specific errors, i.e.,
    ## S[i,j] is the error of a segment from i to j.
    S <- matrix( 0, nrow=m, ncol=m )

    ## T[i,j] is the cost of a segmntation of the range i:m using j segments.
    T <- matrix( 0, nrow=m, ncol=K )
    
    for ( i in 1:m ) {
        ## cat(i, " / ", m, "\n")
        for ( j in i:m ) {
            S[i,j] <- get_tile( i, j )$W
        }
        T[i,1] <- S[i,m]
    }

    ## TI[i,j] is the last (i.e. "rightmost") column of the first segment
    ## when segmenting i:m using j segments.
    TI <- matrix( 0, nrow=m, ncol=K )
    
    for ( k in 2:K ) {
        for ( i in 1:(m-(k-1)) ) {
            tmp      <- sapply( i:(m-(k-1)), function(l) S[i,l] + T[l+1,k-1] )
            T[i, k]  <- max( tmp )
            TI[i, k] <- i + which.max( tmp ) - 1
        }
    }
    list( TI=TI, costs=T[1,] )
})


#' Get segment bounds
#'
#' Get segment bounds
#'
#' @param TI Something
#' @param K is the number of segments.
#'
#' @return The start positions of the segments in an optimal segmentation to K segments.
#'
#' @export 
get_segment_bounds <- function( TI, K ) {
    startpos <- 1
    bounds <- c(startpos)
    for ( j in K:2 ) {
        startpos <- TI[startpos,j] + 1
        bounds <- c( bounds, startpos )
    }
    bounds
}


#' Segments to tiles
#'
#' Takes the bounds output of the dynamic programming algboritm
#' and converts this to a list of tiles. Every tile is a list
#' with components R, a, b, and W, where W is the weight of the tile.
#'
#' @param X the data matrix
#' @param bounds contains the start positions of the segments.
#' @param get_tile Something
#'
#' @return Tiles
#'
#' @export 
segments_to_tiles <- function( X, bounds, get_tile ) {
    n      <- nrow(X)
    tiles  <- list()
    ## add ncol(X)+1 as the starting position of an extra segment to make the code simpler.
    bounds <- c( bounds, ncol(X)+1 )
    tiles  <- lapply( 1:(length(bounds)-1), function(i) {
        a    <- bounds[i]
        b    <- bounds[i+1]-1
        tile <- get_tile( a, b )
        list( R=sort(tile$R), a=a, b=b, weight=tile$W )
    } )
    tiles <- Filter( function(tile) tile$weight>0, tiles )
    tiles <- tiles[ order( sapply( tiles, function(tile) tile$weight ), decreasing=T ) ]
}

#' Max weight tile function
#'
#' Returns a function that takes as arguments the left and right boundary of a tile,
#' and returns a list with components
#' R: the rows that belong to the tile
#' W: the weight of the tile.
#' The function should be used to extract the maximum weight tile
#' from data_matrix with given left and right boundaries
#' subject to prec_lowerbound.
#'
#' @param data_matrix the data matrix
#' @param prec_lwoerbound lower bound on the precision
#'
#' @return Something
#'
#' @export 
max_weight_tile_fnc <- function( data_matrix, prec_lowerbound ) {
    n <- nrow(data_matrix)
    C <- t(apply( data_matrix, 1, cumsum ))
    C <- cbind( rep(0, n), C )
    x <- 1:n
    cmpfun( function( i, j ) {
        seg_len       <- (j - i + 1)
        counts        <- (C[,j+1] - C[,i])
        sorted_counts <- sort( counts, decreasing=TRUE, index.return=TRUE )
        ones          <- cumsum( sorted_counts$x )
        precision     <- ones/(seg_len*x)
        R             <- c()
        if (precision[1] >= prec_lowerbound) {
            if (precision[n] >= prec_lowerbound ) {
                R <- 1:n
            }
            else {
                R <- sorted_counts$ix[ 1:(which( precision < prec_lowerbound )[1] - 1) ]
            }
            R <- Filter( function(i) counts[i]>0, R )
        }
        list( R=R, W=sum(counts[R]) )        
    } )
}


#' Plot segments
#'
#' Plot the input matrix together with the segment boundaries.
#'
#' Takes the bounds output of the dynamic programming algboritm
#' and converts this to a list of tiles. Every tile is a list
#' with components R, a, b, and W, where W is the weight of the tile.
#'
#' @param A a matrix generated e.g. by the make_01model function.
#' @param bounds the list of bounds as returned by the dp_segment function.
#'
#' @return Nothing
#'
#' @export 
plot_segments <- function(A, bounds, ... ) {
    ## quartz()
    ## plot.new()
    image( t(A), asp=dim(A)[1]/dim(A)[2], col=heat.colors(2), ... )
    bounds <- bounds[2:length(bounds)]
    for ( b in bounds/dim(A)[2] ) {
        lines( c(b,b), c(0,1), col='blue', lwd=2 )
    }
}


#' Find global tiles
#'
#' Find global tiles
#'
#' @param X the data matrix
#' @param num_tiles the number of tiles
#' @param precision_lowerbound the lower bound on precision
#'
#' @return The tiling
#'
#' @export 
find_global_tiles <- function( X, num_tiles, precision_lowerbound ) {
    if ( num_tiles > ncol(X) ) {
        stop( "num_tiles cannot be larger than ncol(X)!!" )
    }
    K <- min(2*num_tiles + 1, ncol(X))
    get_tile <- max_weight_tile_fnc(X, precision_lowerbound)
    segmentation <- dp_segment( get_tile, dim(X)[2], K )

    tilings <- lapply( num_tiles:K, function(k) {
        tiling  <- c()
        weight  <- 0
        tile_id <- 0

        bounds <- get_segment_bounds( segmentation$TI, k )
        ## tiles are sorted in decreasing order of weight
        tiles <- segments_to_tiles( X, bounds, get_tile )
        ## cat( 'Got ', length(tiles), ' tiles from segmentation of size ', k, '\n' )

        for ( i in 1:length(tiles) ) {
            tile    <- tiles[[i]]
            tile_id <- tile_id + 1
            weight  <- weight + tile$weight
            tiling  <- rbind( tiling, make_tile( tile$R, tile$a, tile$b, tile_id ) )
        }

        list( tiling=tiling, weight=weight, k=tile_id )
    } )

    tilings <- Filter( function(tiling) tiling$k <= num_tiles, tilings )

    ## sort tilings in decreasing order of weight, and return the first one,
    ## i.e., the heaviest one
    tilings <- tilings[ order(sapply(tilings, function(tiling) tiling$weight),
                              decreasing=TRUE) ]
    tilings[[1]]$tiling
}
