danger <- function(x, k) {
  (x != k) & (k / 2 <= x)
}

bsmote <- function(df, var, k = 5,  over_ratio = 1, method = 1) {
  majority_count <- max(table(df[[var]]))
  ratio_target <- majority_count * over_ratio
  which_upsample <-  which(table(df[[var]]) < ratio_target)
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  min_names <- names(samples_needed)
  out_dfs <- list()
  for (i in seq_along(min_names)) {
    data_mat <- as.matrix(df[names(df) != var])
    ids <- RANN::nn2(data_mat, k = k + 1, searchtype = "priority")$nn.idx
    min_class_in <- df[[var]] == min_names[i]

    danger_ids <- danger(rowSums(matrix((min_class_in)[ids],
                                        ncol = ncol(ids))) - 1, k)

    if (sum(danger_ids) <= k) {
      stop("Not enough danger observations of '", min_names[i],
           "' to perform SMOTE.", call. = FALSE)
    }

    if (method == 1) {
      tmp_df <- as.data.frame(
        smote_data(data_mat[min_class_in, ], k, samples_needed[i],
                   which(danger_ids[min_class_in]))
      )
    }
    if (method == 2) {
      tmp_df <- as.data.frame(
        smote_data(data_mat, k, samples_needed[i], which(danger_ids))
      )
    }

    colnames(tmp_df) <- colnames(data_mat)
    tmp_df[[var]] <- min_names[i]
    out_dfs[[i]] <- tmp_df
  }
  final <- rbind(do.call(rbind, out_dfs), df)
  final[[var]] <- factor(final[[var]])
  rownames(final) <- NULL
  final
}
