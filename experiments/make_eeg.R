subject_id_list <- seq.int(10)
frontal   <-  c("F3_A2", "F4_A1")
central   <- c("C4_A1", "C3_A2")
occipital <- c("O1_A2", "O2_A1")

samples <- 6.5 * 3600 * 200

out_f <- read_discrete_eeg(subject_id_list, channels = frontal, letter = "A", samples)
out_c <- read_discrete_eeg(subject_id_list, channels = central, letter = "A", samples)
out_o <- read_discrete_eeg(subject_id_list, channels = occipital, letter = "A", samples)

eeg_a <- list("frontal" = out_f, "central" = out_c, "occipital" = out_o)

saveRDS(eeg_a, "/tmp/eeg_alpha.rds", compress = "xz")
