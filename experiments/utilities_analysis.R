## Load datasets
load_results <- function(path, dataset_name) {
    file_list <- list.files(path, pattern = dataset_name, full.names = TRUE)

    out <- NULL
    if (length(file_list) > 0) {
        out <- data.frame("dataset" = character(),
                          "algorithm" = character(),
                          "precision_used" = numeric(),
                          "cardinality_set" = numeric(),
                          "precision" = numeric(),
                          "recall" = numeric(),
                          "cardinality" = numeric(),
                          "calculation_time" = numeric())

        for (f in file_list) {
            res <- readRDS(f)

            out <- rbind(out, data.frame(dataset = res$dataset,
                                         algorithm = res$algorithm,
                                         precision_used = res$alpha,
                                         cardinality_set = res$cardinality_set,
                                         precision = res$precision,
                                         recall = res$recall,
                                         cardinality = res$cardinality,
                                         calculation_time = as.numeric(res$calculation_time)))
        }
    }
    out
}



## Get the average running times of the algorithms on the different datasets
running_times <- function(df) {
    ddply(df, .variables = .(algorithm, dataset), .fun = function(tmp_df) data.frame("time" = mean(tmp_df$calculation_time)))
}


## Plot precision_recall-curvesx
plot_pr_curves <- function(df, legend_position = "topright", main) {
    df$cardinality <- df$cardinality_set
    df <- df[order(df$recall),]

    require(ggplot2)
    p <- ggplot(data = df)
    p <- p + geom_path(aes(x = recall, y = precision, color = algorithm, linetype = algorithm, group = interaction(algorithm,cardinality)), size = 0.8)
    p <- p + geom_point(aes(x = recall, y = precision, color = algorithm, group = interaction(algorithm, cardinality), label = cardinality), shape = 21, size = 5, fill = "white")
    p <- p + geom_text(aes(x = recall, y = precision, color = algorithm, group = interaction(algorithm, cardinality), label = cardinality), size = 3)
    p <- p + xlab("Recall")
    p <- p + ylab("Precision")
    p <- p + theme_bw()

    if (legend_position == "topright")
        p <- p + theme(legend.position=c(.80, .85))
    if (legend_position == "bottomleft")
        p <- p + theme(legend.position=c(.2, .15), legend.background = element_rect(fill = "transparent"))

    if (! is.null(main))
        p <- p + ggtitle(main) + theme(plot.title = element_text(face="bold"))

    print(p)
    return(p)

}



## Plot precision_recall-curvesx
plot_cr_curves <- function(df, legend_position = "bottomright", main = NULL) {
    df$cardinality <- df$cardinality_set
    df <- df[order(df$recall),]

    require(ggplot2)
    p <- ggplot(data = df)
    p <- p + geom_path(aes(x = cardinality, y = recall, color = algorithm, linetype = algorithm), size = 2)
    p <- p + geom_point(aes(x = cardinality, y = recall, color = algorithm), shape = 21, size = 2, fill = "gray")
                                        # p <- p + geom_text(aes(x = cardinality, y = recall, color = algorithm, label = cardinality), size = 3)
    p <- p + xlab("Cardinality")
    p <- p + ylab("Recall")
    p <- p + theme_bw()

    if (legend_position == "bottomright")
        p <- p + theme(legend.position=c(.85, .40), legend.background = element_rect(fill = "transparent"))

    if (! is.null(main))
        p <- p + ggtitle(main) + theme(plot.title = element_text(face="bold"))
    
    print(p)
    return(p)

}

plot_result <- function(filename, savename = NULL) {
    res <- readRDS(filename)
    plot_binary_matrix(res$data, res$tiles, main = res$algorithm, savename = savename)
}


