string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

check_na <- function(data, step) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    rlang::abort(paste0(
      "`", step,
      "` cannot have any missing values. NAs found ind: ",
      paste(names(na_cols), collapse = ", "), "."
    ))
  }
}

check_2_levels_only <- function(data, col_name) {
  if (length(levels(data[[col_name]])) != 2) {
    rlang::abort(paste0("`", col_name, "`` must only have 2 levels."))
  }
}


na_splice <- function(new_data, synthetic_data, object) {
  non_predictor <- setdiff(names(new_data), c(object$column, object$predictors))

  if (length(non_predictor) == 0) {
    return(synthetic_data)
  }

  new_data[, non_predictor, drop = FALSE]

  na_data <- matrix(
    nrow = nrow(synthetic_data) - nrow(new_data),
    ncol = length(non_predictor)
  )

  colnames(na_data) <- non_predictor
  na_data <- as.data.frame(na_data)

  res <- bind_cols(
    synthetic_data,
    bind_rows(new_data[, non_predictor, drop = FALSE], na_data)
  )

  res[, names(new_data)]
}
