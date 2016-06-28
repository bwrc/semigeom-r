#' Generates a random 0-1 matrix with some randomly planted
#' segments. Every planted segment has a single subset of rows filled
#' with 1s with probability p. The subset has floor(n/l)
#' rows. Otherwise the matrix is filled with 0s.
#'
#' It might be better in practice to set m to a large enough value,
#' like O(100) or so. The default values give a nice example.
#'
#' @param n Number of rows.
#' @param m Number of columns.
#' @param k Number of randomly planted segments.
#' @param p Probability of 1s in the segments.
#' @param l Span of segment.
#'
#' @return A binary data matrix with k segments.
#'
#' @export
make_01model <- function( n=50, m=100, k=5, p=0.8, l=5 ) {
    boundaries <- c( sort( sample( 10:(m-10), k-1 ) ), m )
    A <- matrix( 0, nrow=n, ncol=m )
    a <- 1
    for ( b in boundaries ) {
        S <- sample( 1:n, floor(n/l) )
        w <- b - a
        A[ S, a:(b-1) ] <- sample( 0:1, floor(n/l)*w, replace=T, prob=c(1-p,p) )
        a <- b
    }
    A
}

#' Generates a random 0-1 matrix with n rows and m columns, and k
#' randomly placed "tiles". Every planted tile has a single subset of
#' l rows that span l consecutive columns.  The matrix is filled with
#' zeros, 1s occur in tiles with probability p.
#'
#' @param n Number of rows.
#' @param m Number of columns.
#' @param k Number of randomly planted tiles.
#' @param p Probability of 1s in the segments.
#' @param l Span of tiles.
#'
#' @return A binary data matrix with k segments.
#'
#' @export
make_01tiles <- function( n=50, m=100, k=5, p=0.8, l=20 ) {
    A <- matrix( 0, nrow=n, ncol=m )
    for ( t.start in sample( 1:(m-l), k) ) {
        S <- sample( 1:n, l )
        A[S, t.start:(t.start+l-1)] <- sample( 0:1, l*l, replace=T, prob=c(1-p,p) )
    }
    A
}


#' Circular shift of array x by n steps.
#'
#' @param x An array.
#' @param n Number of steps for circular shift (positive for forward shift, negative for backward shift)
#'
#' @return Circular-shifted x.
#'
#' @export
circshift <- function(x, n) {
    if (n == 0)
        x
    else
        c(tail(x, n), head(x, -n))
}


#' Shift n randomly chosen rows from the matrix by steps.
#'
#' @param x An array.
#' @param n Number of steps for circular shift.
#'
#' @return Circular-shifted x.
#'
#' @export
random_shift_matrix <- function(x, n, steps = 0, steps_low = NULL, steps_high = NULL, random = FALSE) {
    rows <- sample(nrow(x), n)

    if (random)
        steplist <- ceiling(runif(n, steps_low, steps_high))
    else
        steplist <- rep(steps, n)

    for (i in seq.int(n))
        x[rows[i], ] <- circshift(x[rows[i], ], steplist[i])

    x
}


#' Make list with synthetic datasets
#'
#' @return List with synthetic datasets
#'
#' @export
make_dataset_list_synthetic <- function() {
    require(seriation)
    
    dataset_list <- list()

    ## ------------------------------
    ## (1) Synthetic 1
    set.seed(42)

    x   <- make_01tiles()
    xd  <- dist(x, method = "euclidean")
    xds <- seriate(xd)
    dataset_list[["synthetic1"]]  <- x[get_order(xds),]

    ## ------------------------------
    ## (2) Synthetic 2
    set.seed(42)

    x   <- make_01model()
    xd  <- dist(x, method = "euclidean")
    xds <- seriate(xd)
    dataset_list[["synthetic2"]]  <- x[get_order(xds),]

    ## ------------------------------

    ## Return
    dataset_list

}


#' Randomly shuffle each row of a matrix
#'
#' @param x An array.
#'
#' @return A shuffled vesion of x.
#'
#' @export
shuffle_matrix <- function(x) {
    t(apply(x, 1, function(i) sample(i)))
}
