nearmiss_impl <- function(df, var, ignore_vars, k = 5, under_ratio = 1) {
  classes <- downsample_count(df, var, under_ratio)

  out_dfs <- list()

  deleted_rows <- integer()
  for (i in seq_along(classes)) {
    df_only <- df[, !names(df) %in% ignore_vars]
    class <- subset_to_matrix(df_only, var, names(classes)[i])
    not_class <- subset_to_matrix(df_only, var, names(classes)[i], FALSE)

    if (nrow(not_class) <= k) {
      rlang::abort(paste0(
        "Not enough danger observations of '",
        names(classes)[i],
        "' to perform NEARMISS."
      ))
    }

    dists <- RANN::nn2(
      not_class[, !(colnames(not_class) %in% ignore_vars)],
      class[, !(colnames(class) %in% ignore_vars)],
      k = k
    )$nn.dists

    selected_ind <- order(rowMeans(dists)) <= (nrow(class) - classes[i])
    deleted_rows <- c(deleted_rows, which(df[[var]] %in% names(classes)[1])[!selected_ind])
  }

  if (length(deleted_rows) > 0) {
    df <- df[-deleted_rows, ]
  }
  df
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
