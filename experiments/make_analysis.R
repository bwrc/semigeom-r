source("main.R")
graphics.off()

save_pr_curve <- function(path, dataset_name, legend_position = "topright", main = NULL) {
    res      <- load_results(path = path, dataset_name = dataset_name)
    plot_res <- plot_pr_curves(res, legend_position = legend_position, main = main)
    fig_w <- 5
    fig_h <- 4
    ggsave(filename = paste0("results/pr_", dataset_name, ".pdf"), plot = plot_res, width = fig_w, height = fig_h, units = "in")
}


save_cr_curve <- function(path, dataset_name, legend_position = "bottomright", main = NULL) {
    res      <- load_results(path = path, dataset_name = dataset_name)
    plot_res <- plot_cr_curves(res, legend_position = legend_position, main = main)
    fig_w <- 5
    ggsave(filename = paste0("results/cr_", dataset_name, ".pdf"), plot = plot_res, width = fig_w, height = fig_w/2.5, units = "in")
}

do_pr_curves     <- TRUE
do_cr_curves     <- TRUE
do_plots         <- TRUE
do_running_times <- TRUE

## Produce PR-curves
if (do_pr_curves) {
    save_pr_curve(path = "results/batch_pr/", dataset_name = "synthetic1", legend_position = "bottomleft", main = "synthetic1")
    save_pr_curve(path = "results/batch_pr/", dataset_name = "synthetic2", legend_position = "bottomleft", main = "synthetic2")
    save_pr_curve(path = "results/batch_pr/", dataset_name = "paleo", main = "paleo")
    save_pr_curve(path = "results/batch_pr/", dataset_name = "stockindicesup", main = "stock indices (growth)")
    save_pr_curve(path = "results/batch_pr/", dataset_name = "stockindicesdown", main = "stock indics (decline)")
    save_pr_curve(path = "results/batch_pr/", dataset_name = "stockindicesstable", main = "stock indices (stable)")
    save_pr_curve(path = "results/batch_pr/", dataset_name = "stockprices", main = "stock prices")
}

## Produce CR-curves
if (do_cr_curves) {
    save_cr_curve(path = "results/batch_cr/", dataset_name = "paleo", main = "paleo")
    save_cr_curve(path = "results/batch_cr/", dataset_name = "stockindicesup", main = "stock indices (decline)")
    save_cr_curve(path = "results/batch_cr/", dataset_name = "stockindicesdown", main = "stock indices (growth)")
    save_cr_curve(path = "results/batch_cr/", dataset_name = "stockprices", main = "stock prices")
}



## Plot figures of the various datasets
if (do_plots) {
    ## Synthetic
    plot_result_multi(path = "results/single/", dataset_name = "synthetic1", plot_y_labels = FALSE, figname = "results/synthetic1.pdf")
    plot_result_multi(path = "results/single/", dataset_name = "synthetic2", plot_y_labels = FALSE, figname = "results/synthetic2.pdf")

    ## Paleo
    plot_result_multi(path = "results/single/", dataset_name = "paleo", plot_y_labels = FALSE, xlab = "site", ylab = "species", figname = "results/paleo.pdf")

    ## Stock prices
    res_mt <- readRDS("results/single/MaxTile_stockprices_10_0.3.rds")
    res_gt <- readRDS("results/single/GlobalTile_stockprices_10_0.3.rds")
    res_lt <- readRDS("results/single/LocalTile_stockprices_10_0.3.rds")

    pdf(file = "results/stockprices.pdf", width = 10, height = 5)
    par(mfcol = c(1,3))
    plot_binary_matrix(res_mt$data, res_mt$tiles, xvec = XT, plot_x_labels = TRUE, skipxlabel = 100, main = "MaxTile", plot_y_labels = FALSE, pointcol ="gray")
    plot_binary_matrix(res_gt$data, res_gt$tiles, xvec = XT, plot_x_labels = TRUE, skipxlabel = 100, main = "GlobalTile", plot_y_labels = FALSE, pointcol ="gray")
    plot_binary_matrix(res_lt$data, res_lt$tiles, xvec = XT, plot_x_labels = TRUE, skipxlabel = 100, main = "LocalTile", plot_y_labels = FALSE, pointcol ="gray")
    dev.off()


    ## Stock indices
    source("example_motivating.R")

    ## EEG
    plot_result_multi("results/single/", "eeg_a_s4", prune_singletons = TRUE, figname = "results/eeg_a_s4.pdf", cex.ylab = 2)
    plot_result_multi("results/single/", "eeg_a_s7", prune_singletons = TRUE, figname = "results/eeg_a_s7.pdf", cex.ylab = 2)

}


## Produce a table with the typical running times on the different datasets
if (do_running_times) {
    dataset_names <- c("stockindicesup", "stockindicesdown", "stockprices", "paleo", "eeg_a_s4", "eeg_a_s7", "synthetic1", "synthetic2")
    
    out <- NULL
    for (dsname in dataset_names) {
        if (is.null(out))
            out <- load_results("results/single/", dataset_name = dsname)
        else
            out <- rbind(out, load_results("results/single/", dataset_name = dsname))
    }

    rownames(out) <- NULL
    out <- xtable(out)
    print(out, include.rownames=FALSE)
}
