read_eeg_subject <- function(subject_id, channels, fpath = "../data/eeg/subgroup_3/") {
    fname <- paste0(fpath, "s", subject_id, "/", subject_id, ".rec")
    cat("Reading file: \t", fname, "\n")
    read_data_eeg(fname, channels = channels)
}


read_discrete_eeg <- function(subject_id_list, channels, letter, samples) {
    cat("------------------------------\n")
    cat("Reading channels:\t", channels, "\n")
    cat("Discretizing letter: \t", letter, "\n")

    out <- vector(mode = "list", length = length(subject_id_list))

    for (i in subject_id_list) {
        tmp <- read_eeg_subject(i, channels = channels)
        tmp$data <- tmp$data[,1:samples]
        out[[i]] <- discretize_eeg(tmp$data, fs = tmp$fs, win = 4, short = TRUE)
    }

    tmp <- lapply(out, function(i) discretize_letter(i, letter))
    do.call(rbind, tmp)

}


plot_eeg_res <- function(eeg_res, main) {
    par(mfcol = c(1,3))
    for (i in eeg_res)
        plot_binary_matrix(i, main = main)
}


make_subjectwise_dataset <- function(eeg, subject_id) {
    ## indexing, since there are two rows for each subject
    ind <- 2*subject_id-1

    ## make a new matrix with the frontal, central and occipital channels
    tmp <- rbind(eeg[[1]][ind:(ind+1),], eeg[[2]][ind:(ind+1),],eeg_a[[3]][ind:(ind+1),])
    rownames(tmp) <- paste0(gsub("_A[1-2]", "", rownames(tmp)))
    tmp
}

prune_singletons <- function(tiles) {
    tiles[which(sapply(tiles, function(i) length(i$rows)) > 1)]
}

analyse_eeg_subject <- function(eeg, subject_id, figure_path, save_path, band, algorithm_name, algorithm, dataset_name, cardinality, alpha) {
    fig_fname <- paste0(figure_path, "/", "eeg_", band, "_s", subject_id, ".pdf")
    filename <- paste0(save_path, "/", "eeg_", band, "_s", subject_id, ".rds")    
    cat(filename, "\n")

    data <- make_subjectwise_dataset(eeg, subject_id)
    res <- analyze_and_save(algorithm_name, algorithm, dataset_name, data, alpha, cardinality, filename)

    tiles <- prune_singletons(res$tiles)
    
    pdf(fig_fname, width = 10, height = 4)
    plot_binary_matrix(data, tiles, main = paste0("Subject ", subject_id, " (", band, ")"))
    dev.off()
    
}
