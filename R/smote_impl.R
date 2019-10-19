smote_data <- function(data, k, N) {
  ids <- RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx

  indexes <- rep(seq_len(nrow(data)), each = N)
  out <- matrix(0, nrow = length(indexes), ncol = ncol(data))

  sampleids <- matrix(sample.int(k, nrow(data) * N, TRUE), ncol = N)
  runif_ids <- matrix(stats::runif(nrow(data) * N), ncol = N)
  iii <- 0
  window <- seq_len(N)
  for (i in seq_len(nrow(data))) {
    id_knn  <- ids[i, ids[i, ] != i]
    dif <- data[id_knn[sampleids[i, ]], ] - data[i, ]
    gap <- dif * runif_ids[i, ]
    out[iii + window, ] <- data[i, ] + gap
    iii <- iii + 1
  }
  out
}

smote <- function(df, var, k = 5, N = 1) {
  data <- split(df, df[[var]])

  counts <- vapply(data, nrow, FUN.VALUE = numeric(1))

  minority_classes <- names(counts[max(counts) != counts])

  for (mclass in minority_classes) {
    minority_df <- data[[mclass]]
    minority <- as.matrix(minority_df[names(minority_df) != var])
    synthetic <- smote_data(minority, k = k, N = N)
    out_df <- as.data.frame(rbind(minority, synthetic))
    out_df[var] <- data[[mclass]][[var]][1]
    data[[mclass]] <- out_df[, names(minority_df)]
  }

  final <- do.call(rbind, data)
  final <- final[sample.int(nrow(final)), ]
  rownames(final) <- NULL
  final
}
