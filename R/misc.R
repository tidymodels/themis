string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

check_na <- function(data, step) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    cols <- paste(names(na_cols)[na_cols], collapse = ", ")
    rlang::abort(glue(
      "`{step}` cannot have any missing values. NAs found ind: {cols}."
    ))
  }
}

check_2_levels_only <- function(data, col_name) {
  if (length(levels(data[[col_name]])) != 2) {
    rlang::abort(glue("`{col_name}` must only have 2 levels."))
  }
}

check_numeric <- function(dat) {
  all_good <- vapply(dat, is.numeric, logical(1))
  label <- "numeric"

  if (!all(all_good)) {
    rlang::abort("All columns for this function should be numeric.")
  }
  invisible(all_good)
}

check_column_factor <- function(data, column) {
  if (!is.factor(data[[column]])) {
    rlang::abort(glue("`{column}` should be a factor variable."))
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

#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
