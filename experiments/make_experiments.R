source("main.R")

## Get tiling algorithms
function_list  <- get_function_list()

## Get synthetic datasets
dataset_list_s <- make_dataset_list_synthetic()

## Get real datasets
dataset_list_r <- make_dataset_list_real()

## Cardinalities and alpha step
cardinality_list <- c(3, 7)
alpha_step <- 0.05

## Choose analyses
analyze_synthetic_batch <- FALSE # TRUE
analyze_real_batch      <- FALSE # TRUE

analyze_paleo_batch_cr  <- TRUE
analyze_stockindices_cr <- TRUE
analyze_stockprices_cr  <- TRUE

analyze_synthetic1      <- FALSE # TRUE
analyze_synthetic2      <- FALSE # TRUE
analyze_paleo           <- FALSE # TRUE
analyze_stockprices     <- FALSE # TRUE
analyze_stockindices    <- FALSE # TRUE
analyze_eeg             <- FALSE # TRUE

## Analyze single datasets
if (analyze_synthetic1) {
    analyze_single(function_list, dataset_list_s$synthetic1, K_list = c(5,5), alpha = 0.75, dataset_name = "synthetic1", savepath = "results/", make_plot = TRUE, figname = "results/synthetic1.pdf", plot_y_labels = FALSE)
}

if (analyze_synthetic2) {
    analyze_single(function_list, dataset_list_s$synthetic2, K_list = c(5,5), alpha = 0.75, dataset_name = "synthetic2", savepath = "results/", make_plot = TRUE, figname = "results/synthetic2.pdf", plot_y_labels = FALSE)
}

if (analyze_paleo) {
    K_list <- c(3, 3)
    analyze_single(function_list, dataset_list_r$paleo, K_list = K_list, alpha = 0.3, dataset_name = "paleo", savepath = "results/", make_plot = TRUE, figname = "results/paleo.pdf", plot_y_labels = FALSE)
}

if (analyze_stockprices) {
    K_list <- c(10, 10, 10)
    analyze_single(function_list, dataset_list_r$stockprices, K_list = K_list, alpha = 0.3, dataset_name = "stockprices", savepath = "results/", make_plot = TRUE, figname = "results/stockprices.pdf", plot_y_labels = FALSE)
}


if (analyze_stockindices) {
    K_list <- c(5, 5)
    analyze_single(function_list, dataset_list_r$stockindicesup, K_list = K_list, alpha = 0.85, dataset_name = "stockindicesup", savepath = "results/", make_plot = TRUE, figname = "results/stockindicesup.pdf", plot_y_labels = FALSE)
    analyze_single(function_list, dataset_list_r$stockindicesdown, K_list = K_list, alpha = 0.85, dataset_name = "stockindicesdown", savepath = "results/", make_plot = TRUE, figname = "results/stockindicesdown.pdf", plot_y_labels = FALSE)
}

if (analyze_eeg) {
    ## Analyze two of the EEG datasets
    eeg_a <- readRDS("eeg_alpha.rds")
    eeg_a_s04 <- make_subjectwise_dataset(eeg_a, 4)
    eeg_a_s07 <- make_subjectwise_dataset(eeg_a, 7)
    
    save_path <- "results_"
    K_list <- c(10, 10, 10)
    analyze_single(function_list, eeg_a_s04, K_list = K_list, alpha = 0.6, dataset_name = "eeg_a_s4", savepath = "results/", make_plot = TRUE, figname = "results/eeg_a_s4.pdf", plot_y_labels = FALSE)
    analyze_single(function_list, eeg_a_s07, K_list = K_list, alpha = 0.6, dataset_name = "eeg_a_s7", savepath = "results/", make_plot = TRUE, figname = "results/eeg_a_s7.pdf", plot_y_labels = FALSE)

}


## Analyze batch datasets with varying parameters
if (analyze_synthetic_batch) {
    analyze_batch(function_list, dataset_list_s, cardinality_list, alpha_step, savepath = "results/")
}


if (analyze_real_batch) {
    analyze_batch(function_list, dataset_list_r, cardinality_list, alpha_step, savepath = "results/")
}

if (analyze_paleo_batch_cr) {
    set.seed(42)
    for (i in seq.int(2,60))
        analyze_single(function_list, dataset_list_r$paleo, K_list = rep(i, 3), alpha = 0.50, dataset_name = "paleo", savepath = "results/", make_plot = FALSE, figname = NULL)
}

if (analyze_stockindices_cr) {
    set.seed(42)
    for (i in seq.int(2,30))
        analyze_single(function_list, dataset_list_r$stockindicesup, K_list = rep(i, 3), alpha = 0.85, dataset_name = "stockindicesup", savepath = "results/", make_plot = FALSE, figname = NULL)

    for (i in seq.int(2,30))
        analyze_single(function_list, dataset_list_r$stockindicesdown, K_list = rep(i, 3), alpha = 0.85, dataset_name = "stockindicesdown", savepath = "results/", make_plot = FALSE, figname = NULL)
    
}

if (analyze_stockprices_cr) {
    set.seed(42)
    for (i in seq.int(2,100))
        analyze_single(function_list, dataset_list_r$stockprices, K_list = rep(i, 3), alpha = 0.3, dataset_name = "stockprices", savepath = "results/", make_plot = FALSE, figname = NULL)

}
