source("main.R")

require(TTR)
require(xts)

syms <- TTR::stockSymbols()

sectors         <- c("Finance", "Consumer Services", "Technology", "Health Care")
begin           <- 20110103
end             <- 20151231
N               <- 100  ## number of stocks per sector
n_days_expected <- 1258 ## empirically tested

## Choose symbols
syms_tmp <- subset(syms, (Sector %in% sectors) & (IPOyear < 2011))

## Get the data
out   <- c()
out_t <- c()

out   <- matrix(nrow = length(sectors) * N, ncol = n_days_expected)
out_t <- matrix(nrow = length(sectors) * N, ncol = n_days_expected)

rownames(out)   <- rep("", nrow(out))
rownames(out_t) <- rep("", nrow(out_t))

if (TRUE) {
    set.seed(42)
    row_index <- 1
    for (sector in sectors) {
        stock_symbols <- sample(syms_tmp$Symbol[syms_tmp$Sector == sector]) ## choose in random order
        i <- 1
        j <- 0
        while ((i < length(stock_symbols)) && (j < N)) {
            data <- getYahooData(stock_symbols[i], begin, end)

            if (length(data$Close) == n_days_expected) {
                j <- j + 1
                
                cat(row_index, " / ", nrow(out), "\t\t", j, " / ", N, "\t", stock_symbols[i], "\t", sector, "\n")
                
                out[row_index,]            <- as.numeric(data$Close)
                out_t[row_index,]          <- index(data)

                rownames(out)[row_index]   <- stock_symbols[i]
                rownames(out_t)[row_index] <- stock_symbols[i]

                row_index <- row_index + 1

            }
            i <- i + 1
        }
        cat("\n")
    }
}

## Discretize the stock price data
rolling_percentage <- function(x) {
    100 * ((x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)])
}


stockdata <- list()
stockdata$closingprice <- out
stockdata$time <- as.Date(out_t[1,])
stockdata$closingprice_pc <- t(apply(out, 1, rolling_percentage))
stockdata$time_pc <- as.Date(out_t[1,-1])

## Add metadata
stockdata$meta <- subset(syms, Symbol %in% rownames(out))

## Save dataset
saveRDS(stockdata, file = "/tmp/stockdata.rds", compress = "xz")


