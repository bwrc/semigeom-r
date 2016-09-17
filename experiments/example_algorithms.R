source("main.R")

## create data
X <- matrix(0, nrow = 10, ncol = 10)

## insert submatrices
insert_submatrix <- function(x, r, c) {
    x[r,c] <- 1
    x
}

olap <- 2.5

X <- insert_submatrix(X, 1:5, 1:5)
X <- insert_submatrix(X, olap + (2:5), olap + (2:5))
X <- insert_submatrix(X, olap*2 + (2:5), olap*2 + (2:5))


## Find tiling
alpha <- 0.95
N <- 5

tiles_mt <- max_tile(X, N, alpha = alpha)
tiles_gt <- global_tile(X, N, alpha = alpha)

## Performance metrocs
pt_mt <-  perf_all_metrics(X, tiles_mt, as_string = TRUE)
pt_gt <-  perf_all_metrics(X, tiles_gt, as_string = TRUE)

pdf(file = "results/example_algorithms.pdf", width = 10, height = 3.5)
par(mfrow = c(1, 3))
plot_binary_matrix(X, main = "Original Data")
plot_binary_matrix(X, tiles_mt, main = "MaxTile")
plot_binary_matrix(X, tiles_gt, main = "GlobalTile")
dev.off()
