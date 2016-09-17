## Analyze a dataset and save the result
analyze_and_save <- function(algorithm_name, algorithm, dataset_name, data, alpha, cardinality, filename) {

    t_start          <- Sys.time()
    tiles            <- algorithm(X = data, K = cardinality, alpha = alpha)
    calculation_time <- difftime(Sys.time(), t_start, units = "secs")

    res <- perf_all_metrics(data, tiles)

    tmp <- list("algorithm" = algorithm_name,
                "dataset" = dataset_name,
                "data" = data,
                "alpha" = alpha,
                "tiles" = tiles,
                "precision" = res$precision,
                "recall" = res$recall,
                "cardinality" = res$cardinality,
                "cardinality_set" = cardinality,
                "calculation_time" = calculation_time)

    saveRDS(tmp, file = filename)
    tmp
}


## Batch analysis of datasets
analyze_batch <- function(function_list, dataset_list, cardinality_list, alpha_step, savepath, seed = 42) {
    for (ind_d in seq.int(length(dataset_list))) {
        X <- dataset_list[[ind_d]]
        for (ind_f in seq.int(length(function_list))) {
            for (k in cardinality_list) {
                for (alpha in seq(from = 0, to = 1, by = alpha_step)) {

                    ## variables
                    algorithm_name <- names(function_list)[ind_f]
                    dataset_name   <- names(dataset_list)[ind_d]

                    cat("Dataset: ", dataset_name, "\tAlgorithm: ", algorithm_name, "\tN: ", k, "\talpha: ", alpha, "\n")

                    fname <- file.path(savepath, paste0(algorithm_name, "_", dataset_name, "_", k, "_", alpha, ".rds"))

                    ## analyze dataset and save result
                    set.seed(seed)
                    tmp <- analyze_and_save(algorithm_name = algorithm_name,
                                            algorithm = function_list[[ind_f]],
                                            dataset = dataset_name,
                                            data = X,
                                            alpha = alpha,
                                            cardinality = k,
                                            filename = fname)
                }
            }
        }
    }
}


## Analyze a single dataset
analyze_single <- function(function_list, X, K_list, alpha, dataset_name, savepath, make_plot = FALSE, figname = NULL, seed = 42, plot_y_labels = TRUE) {

    in_plot <- FALSE

    for (ind_f in seq.int(length(function_list))) {
        ## Set random seed
        set.seed(seed)

        ## variables
        algorithm_name <- names(function_list)[ind_f]

        K <- K_list[ind_f]

        cat("Dataset: ", dataset_name, "\tAlgorithm: ", algorithm_name, "\tN: ", K, "\talpha: ", alpha, "\n")

        fname <- file.path(savepath, paste0(algorithm_name, "_", dataset_name, "_", K, "_", alpha, ".rds"))

        ## analyze dataset and save result
        res <- analyze_and_save(algorithm_name = algorithm_name,
                                algorithm = function_list[[ind_f]],
                                dataset = dataset_name,
                                data = X,
                                alpha = alpha,
                                cardinality = K,
                                filename = fname)

        if (make_plot) {
            if (! in_plot) {
                in_plot <- TRUE

                if (! is.null(figname)) {
                    pdf(figname, height = 6, width = 6 * length(function_list))
                }

                par(mfcol = c(1, length(function_list)))

            }

            plot_binary_matrix(res$data, res$tiles, main = res$algorithm, plot_y_labels = plot_y_labels)
        }

    }

    if ((in_plot) & (! is.null(figname)))
        dev.off()
}

