## Reformat stock index data

library(gdata)

read_time_vector <- function(fname) {
    df <- read.xls(fname, sheet = 2, header = FALSE)
    strftime(strptime(df$V1[6:nrow(df)], format = "%Y-%m-%d"), "%m/%d/%Y")

}

read_data_vector <- function(fname, sheet_index) {
    df <- read.xls(fname, sheet = sheet_index, header = FALSE)
    list("name" = gsub("&", "AND", as.character(df$V2[5])), "data" = as.numeric(as.character(df$V2[6:nrow(df)])))
}

generate_stock_price_dataset <- function(fname) {
    clabs <- rep('', 11)
    clabs[1] <- "Date"

    time_vec <- read_time_vector(fname)
    df <- data.frame(time_vec)
    for (i in seq.int(2, 11)) {
        tmp <- read_data_vector(fname, i)
        df <- cbind(df, tmp$data)
        clabs[i] <- tmp$name
    }
    colnames(df) <- clabs
    df
}

fname <- "/tmp/stockindexes.xls"
tmp <- generate_stock_price_dataset(fname)
write.table(tmp, "/tmp/stock_exchange_index.csv", row.names = FALSE, quote = FALSE, sep = ";")
