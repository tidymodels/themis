nearmiss <- function(df, var, k = 5, under_ratio = 1) {
  classes <- downsample_count(df, var, under_ratio)

  out_dfs <- list()
  for (i in seq_along(classes)) {
    class <- subset_to_matrix(df, var, names(classes)[i])
    not_class <- subset_to_matrix(df, var, names(classes)[i], FALSE)

    dists <- RANN::nn2(not_class, class, k = k)$nn.dists

    selected_rows <- class[order(rowMeans(dists)) <=
                             (nrow(class) - classes[i]), ]
    out_df <- as.data.frame(selected_rows)
    out_df[var] <- names(classes)[i]
    out_dfs[[i]] <- out_df[, names(df)]
  }

  out_dfs[[i + 1]] <- df[!(df[[var]] %in% names(classes)), ]

  final <- do.call(rbind, out_dfs)
  rownames(final) <- NULL
  final
}

downsample_count <- function(data, var, ratio) {
  min_count <- min(table(data[[var]]))
  ratio_target <- min_count * ratio
  which_class <- which(table(data[[var]]) > ratio_target)
  table(data[[var]])[which_class] - ratio_target
}

subset_to_matrix <- function(data, var, class, equal = TRUE) {
  if (equal) {
    return(as.matrix(data[data[[var]] == class, names(data) != var]))
  } else {
    return(as.matrix(data[data[[var]] != class, names(data) != var]))
  }
}
