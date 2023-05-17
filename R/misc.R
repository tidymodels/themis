string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

check_na <- function(data, step, call = caller_env()) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    cols <- paste(names(na_cols)[na_cols], collapse = ", ")
    rlang::abort(
      glue(
        "Cannot have any missing values. NAs found ind: {cols}."
      ),
      call = call
    )
  }
}

check_2_levels_only <- function(data, col_name, call = caller_env()) {
  if (length(levels(data[[col_name]])) != 2) {
    rlang::abort(glue("`{col_name}` must only have 2 levels."), call = call)
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

check_column_factor <- function(data, column, call = caller_env()) {
  if (!is.factor(data[[column]])) {
    rlang::abort(glue("`{column}` should be a factor variable."), call = call)
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

  res <- vec_cbind(
    synthetic_data,
    bind_rows(new_data[, non_predictor, drop = FALSE], na_data)
  )

  res <- res[, names(new_data)]

  as_tibble(res)
}

#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

weighted_table <- function(x, wts = NULL) {
  if (is.null(wts)) {
    wts <- rep(1, length(x))
  }

  if (!is.factor(x)) {
    x <- factor(x)
  }

  hardhat::weighted_table(x, weights = wts)
}

get_from_info <- function(info, role, na_rm = TRUE) {
  res <- info$variable[info$role == role]

  if (na_rm) {
    res <- stats::na.omit(res)
  }
  res
}
