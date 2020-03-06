smote_data <- function(data, k, n_samples, smote_ids = seq_len(nrow(data))) {
  ids <- RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx
  indexes <- rep(sample(smote_ids), length.out = n_samples)
  index_len <- tabulate(indexes, NROW(data))
  out <- matrix(0, nrow = n_samples, ncol = ncol(data))
  sampleids <- sample.int(k, n_samples, TRUE)
  runif_ids <- stats::runif(n_samples)

  iii <- 0
  for (row_num in smote_ids) {
    index_selection <- iii + seq_len(index_len[row_num])
    # removes itself as nearest neighbour
    id_knn <- ids[row_num, ids[row_num, ] != row_num]
    dif <- data[id_knn[sampleids[index_selection]], ] -
      data[rep(row_num, index_len[row_num]), ]
    gap <- dif * runif_ids[index_selection]
    out[index_selection, ] <- data[rep(row_num, index_len[row_num]), ] + gap
    iii <- iii + index_len[row_num]
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
    out_df[var] <- data[[names(samples_needed)[i]]][[var]][1]
    data[[names(samples_needed)[i]]] <- out_df[, names(minority_df)]
  }

  final <- do.call(rbind, data)
  rownames(final) <- NULL
  final
}
