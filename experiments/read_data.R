## ==============================================================================
## Read data: Stock indices
## ==============================================================================

#' Read Stock Index data from
#' http://www.economicswebinstitute.org/ecdata.htm
#' http://www.economicswebinstitute.org/data/stockindexes.zip
#'
#' This function reads stock index data from a file and
#' returns time series data as a matrix.
#'
#' @param fname Name of the data file with the data.
#'
#' @return A matrix containing the stock index.
#'
#' @export
read_data_stock_indices <- function(fname = "data/stock_exchange_index.csv", return_date = FALSE) {
    tmp <- read.csv(fname, header = TRUE, sep = ";")

    datevec    <- strptime(tmp$Date, format = "%m/%d/%Y")
    tmp$Date   <- NULL
    tmp        <- as.matrix(tmp)
    if (return_date)
        list("date" = datevec, "data" = t(tmp))
    else
        t(tmp)
}

## ==============================================================================
## Read data: Paleo dataset (g10s10)
## ==============================================================================

#' Read Paleo dataset (g10s10) from
#' http://www.cis.hut.fi/projects/patdis/paleo/
#'
#' This function reads the Paleo dataset.
#'
#' @param fname Name of the data file with the data.
#' @return A binary matrix with sites on the rows and species on the columns.
#'         A 1 indicates that a particular species was found in that location
#'         (otherwise 0).
#'
#' @export
read_data_paleo <- function(fname = "data/g10s10.txt") {
    f_rn <- gsub(".txt", ".sites", fname)
    f_cn <- gsub(".txt", ".genus", fname)

    tmp           <- matrix(scan(fname, quiet = TRUE, what = integer()), nrow = 124, ncol = 139, byrow = TRUE)

    rn_tmp <- scan(f_rn, quiet = TRUE, what = character(), sep = "\n", fileEncoding = "ISO-8859-1")
    rn_tmp <- sapply(rn_tmp, function(i) strsplit(i, split = " ")[[1]][1], USE.NAMES = FALSE)

    rownames(tmp) <- rn_tmp
    colnames(tmp) <- scan(f_cn, quiet = TRUE, what = character())

    t(tmp)
}

## ==============================================================================
## Read data: Stock Prices
## ==============================================================================

## Read stock price data from a file, generated using the 
read_data_stock_prices <- function() {
    readRDS("data/stockdata.rds")
}

get_tdi <- function(x) {
    tmp <- TDI(x, n = 30)
    tmp <- apply(sign(tmp), 1, prod)
    tmp[tmp < 0] <- 0
    tmp
}

get_aroon <- function(x) {
    tmp <- aroon(x, n = 30)
    out <- 0 * x
    out[tmp[,1] >= 70] <- 1
    out[tmp[,2] >= 70] <- -1
    out
}

get_rsi <- function(x) {
    tmp <- RSI(x, n=14, maType = "WMA")
    out <- 0 * x
    out[tmp > 70] <- 1
    out[tmp < 30] <- -1
    out
}

get_cci <- function(x) {
    tmp <- CCI(x, n=20)
    out <- 0 * x
    out[tmp > 100] <- 1
    out[tmp < -100] <- -1
    out
}

get_sax <- function(alphabet_size = 2, PAA_number = 100) {
    function(x) {
        seewave::SAX(x, alphabet_size = alphabet_size, PAA_number = PAA_number)
    }
}

get_nasdaq_difference <- function(x) {
    n <- attr(get_nasdaq_difference, "data")
    if(is.null(n)){
        n <- as.numeric(TTR::getYahooData("^IXIC", 19930101,20110101)$Close)
        n <- n / n[1] # start the index at 1
        attr(get_nasdaq_difference, "data") <<- n
    }
    x <- x / x[1] # start the stock at 1
    tmp <- x * 0
    tmp[x > n] = 1
    tmp[x < n] = -1
    tmp[abs(x - n) < 0.025]
    tmp
}




## ==============================================================================
## Read data: EEG data
## ==============================================================================

#' Transform recording to a matrix.
#'
#' Transforms a recording with physiologic data into a matrix.
#'
#' @param recording A recording structure
#' @param channels A list with channel names
#'
#' @return The channel data as a matrix
#'
#' @export
recording_to_matrix <- function(recording, channels) {
    nr  <- length(channels)
    nc  <- length(recording$signal[[channels[1]]]$data)
    out <- matrix(NA, nrow = nr, ncol = nc)

    for (i in seq.int(length(channels)))
        out[i,] <- recording$signal[[channels[i]]]$data

    rownames(out) <- channels

    out

}


#' Read EEG data.
#'
#' Read EEG data in the EDF format.
#'
#' @param fname Name of the data file with the data (in the EDF format).
#'
#' @return A list with two elements: (i) a matrix with the EEG data
#'     channels and (ii) the sampling rate in Hertz.
#'
#' @export
read_data_eeg <- function(fname = "data/eeg/1.rec", channels = c("F3_A2", "C3_A2",  "O1_A2",  "F4_A1",  "C4_A1",  "O2_A1" )) {
    ## Read header information
    require(edf)
    recording <- read.edf(fname)

    ## Get sampling rate
    fs <- recording$header.signal[[channels[1]]]$samplingrate

    ## Transform the data into a matrix
    data <- recording_to_matrix(recording, channels)

    ## Return data
    list("data" = data, "fs" = fs)

}


## ==============================================================================
## Helper functions
## ==============================================================================

#' Discretize the data according to the row or column mean.
#'
#' This function discretizes data in a matrix according to (i) row or
#' column means (default), corresponding to \code{type} 'rows' and
#' 'columns'. A value of 1 denotes that the value is above the
#' row/column mean and 0 that it is below. If \code{type} is
#' 'derivative' the discretisation is made according to the change
#' from first to last value in a short window of the time series.
#'
#' @param data Data matrix, time series on the rows.
#'
#' @param type The type of discreatisation: rows, columns, derivative
#'     or scale.
#'
#' @param thr Threshold for considering data as unchanged for
#'     \code{type} 'derivative'. Default is 0.025.
#'
#' @param win Window for discretisation when using \code{type}
#'     'derivative'. Default is 5.
#'
#' @return The discretized data matrix.
#'
#' @export
discretize_data <- function(data, type = "columns", thr = 0.025, win = 5) {
    if (type == "columns") {
        colm          <- colMeans(data)
        tmp           <- sapply(seq.int(ncol(data)), function(i) as.numeric(data[,i] > colm[i]))
        rownames(tmp) <- rownames(data)
    }

    if (type == "rows") {
        rowm          <- rowMeans(data)
        tmp           <- t(sapply(seq.int(nrow(data)), function(i) as.numeric(data[i,] > rowm[i])))
        rownames(tmp) <- rownames(data)
    }

    if (type == "derivative") {
        tmp <- change_direction_matrix(data, thr = thr, win = win)
    }

    if (type == "scale") {
        tmp <- t(scale(t(data)))
    }

    tmp
}


#' Calculate the change between the first and last values of a vector.
#'
#' This function discretizes data in a vector according to the change
#' from the first to the last value. A zero denotes that the data
#' changed less than \code{thr}. Default is 0.025, i.e., a change of
#' 2.5 percent. A 1 denotes that the change was positive and -1 that
#' the change was negative.
#'
#' @param data Data matrix, time series on the rows.
#' @param thr Threshold for considering data as unchanged. Default is 0.025.
#' @return An integer value (-1, 0, 1) denoting the direction of change.
#'
#' @export
change_direction <- function(x, thr = 0.025) {
    slope <- coef(lm(x~seq.int(length(x))))[[2]]
    slope <- unname(slope)

    if (abs(slope) <= thr)
        0
    else
        sign(slope)
}


#' Calculate change between the first and last values in short time
#' windows for a matrix.
#'
#' This function calculate change between the first and last values in short time
#' windows for a matrix. See \code{change_direction} for details, using windows of width \code{win}.
#'
#' @param data Data matrix, time series on the rows.
#'
#' @param thr Threshold for considering data as unchanged. Default is 0.025.
#'
#' @param win Window for discretisation when using \code{type}
#'     'derivative'. Default is 5.
#'
#' @return An integer value (-1, 0, 1) denoting the direction of
#'     change.
#'
#' @export
change_direction_matrix <- function(x, thr = 0.025, win = 5) {
    out <- t(sapply(seq.int(nrow(x)),
                    function(i) sapply(seq.int(from = 1, to = (ncol(x)-win+1), by = win),
                                       function(j) change_direction(x[i, j:(j+win-1)], thr = thr))))

    rownames(out) <- rownames(x)
    colnames(out) <- colnames(x)[seq.int(from = 1, to = ncol(x), by = win)]

    out
}

## ==============================================================================
## Functions for spectrum estimation and EEG band classification
## ==============================================================================

#' Spectrum estimation using the FFT.
#'
#' Estimate the power spectrum using the FFT.
#'
#' @param x A vector with data.
#' @param fs The sampling rate of the data.
#'
#' @return A list with the components \code{P}: power spectral density
#'     and \code{f}: the frequencies.
#'
#' @export
estimate_spectrum <- function(x, fs) {
    x   <- x - mean(x)

    Y   <- fft(x)
    Pyy <- abs(Y * Conj(Y)) / length(x)
    Pyy <- Pyy[1:  floor(length(Pyy) / 2)]
    f   <- seq.int(from = 0, to = length(Pyy)-1) * (fs / length(x))

    list("P" = Pyy, "f" = f)

}


#' Find dominant frequency in the spectrum.
#'
#' Find the frequency corresponding to the highest power in the
#' spectrum.
#'
#' @param f The frequency vector.
#' @param P The power spectral density at the frequencies given by f.
#'
#' @return The frequency corresponding to the highest power in the
#'     spectrum.
#'
#' @export
dominant_frequency <- function(f, P) {
    f[which.max(P)]
}


#' Find dominant EEG frequency band.
#'
#' Map dominant frequency to named EEG bands (alpha, delta, etc).
#'
#' @param f The frequency vector.
#' @param P The power spectral density at the frequencies given by f.
#'
#' @param short Should the bands be abbreviated to one letter only
#'     (default is \code{FALSE}).
#'
#' @return The dominant EEG frequency band as a string or letter.
#'
#' @export
dominant_eeg_band <- function(f, P, short = FALSE) {
    bands <- list("Delta" = c(1, 4),
                  "Theta" = c(4, 7),
                  "Alpha" = c(8, 13),
                  "Sigma" = c(13, 16),
                  "Beta" = c(17, 35))

    bandpower <- sapply(bands, function(i) integrate_power(f, P, i[1], i[2]))
    out <- names(which.max(bandpower))

    if (short)
        substr(out, 1, 1)
    else
        out

}


#' Determine dominant EEG frequency band.
#'
#' Determine dominant EEG frequency band.
#'
#' @param x Data vector
#'
#' @param fs The sampling rate of the data in Hertz.
#'
#' @return The dominant EEG frequency band as a string or letter.
#'
#' @export
discretize_eeg_helper <- function(x, fs) {
    tmp <- estimate_spectrum(x, fs)
    dominant_eeg_band(tmp$f, tmp$P, short = TRUE)
}


integrate_power <- function(f, p, fmin, fmax) {
    if (length(fmin) == 2) {
        fmax <- fmin[2]
        fmin <- fmin[1]
    }

    imin <- head(which(f >= fmin), 1)
    imax <- head(which(f >= fmax), 1)

    if (FALSE) {
        h <- (fmax - fmin) / (imax - imin)
        n <- length(p)

        h * sum(p[imin:(imax-1)] + p[(imin + 1):(imax)]) / 2
    }
    require(caTools)
    trapz(f[imin:imax], p[imin:imax])
}


#' Calculate change between the first and last values in short time
#' windows for a matrix.
#'
#' This function calculate change between the first and last values in short time
#' windows for a matrix. See \code{change_direction} for details, using windows of width \code{win}.
#'
#' @param x Data matrix, one time series on the rows.
#'
#' @param fs The sampling rate in Hertz.
#'
#' @param win Window for discretisation. Set this to the number of
#'     samples needed for some reasonable spectrum estimation (e.g.,
#'     the number of samples in 1 second of data).
#'
#' @param short Should the bands be abbreviated to one letter only
#'     (default is \code{FALSE}).
#'
#' @return A character matrix with the alphabetised EEG data.
#'
#' @export
discretize_eeg <- function(x, fs, win = 1, short = TRUE) {
    out <- t(sapply(seq.int(nrow(x)),
                    function(i) sapply(seq.int(from = 0, to = (ncol(x)-fs*win), by = (fs*win)),
                                       function(j) discretize_eeg_helper(x[i,(j+1):(j + fs*win)], fs = fs))))

    rownames(out) <- rownames(x)

    out
}

make_dataset_list_real <- function() {
    dataset_list <- list()

    ## ------------------------------
    ## (2) Paleo data
    dataset_list[["paleo"]] <- read_data_paleo()
    ## ------------------------------

    ## ------------------------------
    ## (3) Stock indices
    x <- read_data_stock_indices()
    xo <- discretize_data(x, type = "derivative", win = 30)

    dataset_list[["stockindicesup"]] <- discretize_letter(xo, letter = 1)
    dataset_list[["stockindicesdown"]] <- discretize_letter(xo, letter = -1)
    dataset_list[["stockindicesstable"]] <- discretize_letter(xo, letter = 0)
    ## ------------------------------

    ## ------------------------------
    ## (4) Stock prices
    stockdata <- readRDS("data/stockdata.rds")
    tmp <- 0 * stockdata$closingprice_pc
    ind <- which(stockdata$closingprice_pc < -6)
    tmp[ind] <- 1
    dataset_list[["stockprices"]] <- tmp
    ## ------------------------------

    ## Return
    dataset_list

}
