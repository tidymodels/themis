string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

check_na <- function(data, step) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    rlang::abort(paste0("`", step,
                        "` cannot have any missing values. NAs found ind: ",
                 paste(names(na_cols), collapse = ", "), "."))
  }
}

check_2_levels_only <- function(data, col_name) {
  if (length(levels(data[[col_name]])) != 2)
    rlang::abort(paste0("`", col_name, "`` must only have 2 levels."))
}
