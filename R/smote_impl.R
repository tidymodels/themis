smote_data <- function(data, k, n_samples) {
  ids <- RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx
  indexes <- rep(sample(seq_len(nrow(data))), length.out = n_samples)
  out <- matrix(0, nrow = length(indexes), ncol = ncol(data))
  sampleids <- sample.int(k, length(indexes), TRUE)
  runif_ids <- stats::runif(length(indexes))

  for (row_num in seq_len(nrow(data))) {
    index_selection <- indexes == row_num
    # removes itself as nearest neighbour
    id_knn  <- ids[row_num, ids[row_num, ] != row_num]
    dif <- data[id_knn[sampleids[index_selection]], ] - data[row_num, ]
    gap <- dif * runif_ids[index_selection]
    out[index_selection, ] <- data[row_num, ] + gap
  }

  out
}

smote <- function(df, var, k = 5, over_ratio = 1) {
  data <- split(df, df[[var]])
  majority_count <- max(table(df[[var]]))
  ratio_target <- majority_count * over_ratio
  which_upsample <-  which(table(df[[var]]) < ratio_target)
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  min_names <- names(samples_needed)

  for (i in seq_along(samples_needed)) {
    minority_df <- data[[min_names[i]]]
    minority <- as.matrix(minority_df[names(minority_df) != var])
    synthetic <- smote_data(minority, k = k, n_samples = samples_needed[i])
    out_df <- as.data.frame(rbind(minority, synthetic))
    out_df[var] <- data[[i]][[var]][1]
    data[[i]] <- out_df[, names(minority_df)]
  }

  final <- do.call(rbind, data)
  final <- final[sample.int(nrow(final)), ]
  rownames(final) <- NULL
  final
}
