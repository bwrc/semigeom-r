## Plot results for the EEG dataset
source("main.R")

dset1 <- list(
    "eeg_s4_mt" = readRDS("data/MaxTile_eeg_a_s4_10_0.6.rds"),
    "eeg_s4_gt" = readRDS("data/GlobalTile_eeg_a_s4_10_0.6.rds"))
dset2 <- list(
    "eeg_s7_mt" = readRDS("data/MaxTile_eeg_a_s7_10_0.6.rds"),
    "eeg_s7_gt" = readRDS("data/GlobalTile_eeg_a_s7_10_0.6.rds"))

## Construct the plot
pdf("results/eeg_a_s4_1.pdf", width = 6, height = 3.7)
par(mfcol = c(1, 2))
for (i in dset1)
    plot_binary_matrix(i$data, i$tiles, main = i$algorithm, cex.main = 1)
dev.off()

pdf("results/eeg_a_s7_1.pdf", width = 6, height = 3.7)
par(mfcol = c(1, 2))
for (i in dset2)
    plot_binary_matrix(i$data, i$tiles, main = i$algorithm, cex.main = 1)
dev.off()
