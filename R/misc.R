string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

check_na <- function(data, call = caller_env()) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    cols <- paste(names(na_cols)[na_cols], collapse = ", ")
    rlang::abort(
      glue(
        "Missing values are not supported. NAs found ind: {cols}."
      ),
      call = call
    )
  }
}

check_2_levels_only <- function(data, col_name, call) {
  if (length(levels(data[[col_name]])) != 2) {
    rlang::abort(
      glue("`{col_name}` must only have 2 levels."),
      call = call
    )
  }
}

check_numeric <- function(dat, call = caller_env()) {
  all_good <- vapply(dat, is.numeric, logical(1))
  label <- "numeric"

  if (!all(all_good)) {
    rlang::abort(
      "All columns for this function should be numeric.",
      call = call
    )
  }
}

check_column_factor <- function(data, column, call) {
  if (!is.factor(data[[column]])) {
    rlang::abort(
      glue("`{column}` should be a factor variable."),
      call = call
    )
  }
}

check_at_most_one <- function(col_name, call) {
  if (length(col_name) > 1) {
    rlang::abort(
      "The selector should select at most a single variable",
      call = call,
    )
  }
}

check_type <- function(dat, quant = TRUE, call) {
  if (quant) {
    all_good <- vapply(dat, is.numeric, logical(1))
    label <- "numeric"
  } else {
    all_good <- vapply(dat, is_qual, logical(1))
    label <- "factor or character"
  }
  if (!all(all_good))
    rlang::abort(
      glue("All columns selected for the step should be {label}."),
      call = call
    )
  invisible(all_good)
}

is_qual <- function(x) {
  is.factor(x) | is.character(x)
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
