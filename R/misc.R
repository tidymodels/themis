string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

check_na <- function(data, call = caller_env()) {
  na_cols <- vapply(data, function(x) any(is.na(x)), FUN.VALUE = logical(1))
  if (any(na_cols)) {
    cols <- paste(names(na_cols)[na_cols], collapse = ", ")
    cli::cli_abort(
      "Cannot have any missing values. NAs found in {cols}.",
      call = call
    )
  }
}

check_2_levels_only <- function(data, col_name, call = caller_env()) {
  if (
    length(col_name) == 1 &&
      length(levels(drop_unused_levels(data[[col_name]]))) != 2
  ) {
    cli::cli_abort(
      "The {.code {col_name}} must only have 2 levels.",
      call = call
    )
  }
}

check_1_selected <- function(x, call = caller_env()) {
  if (length(x) > 1) {
    cli::cli_abort(
      "The selector should select at most a single variable.",
      call = call
    )
  }
}

check_numeric <- function(dat, call = caller_env()) {
  all_good <- vapply(dat, is.numeric, logical(1))

  if (!all(all_good)) {
    bad_cols <- names(all_good)[!all_good]
    cli::cli_abort(
      "All columns for this function should be numeric.
       {cli::qty(length(bad_cols))} Non-numeric column{?s} found: {.var {bad_cols}}.",
      call = call
    )
  }
  invisible(all_good)
}

check_all_categorical <- function(dat, call = caller_env()) {
  all_good <- vapply(
    dat,
    function(x) is.factor(x) || is.character(x),
    logical(1)
  )

  if (!all(all_good)) {
    bad_cols <- names(all_good)[!all_good]
    cli::cli_abort(
      "All predictor columns for this function should be categorical
       (factor or character). {cli::qty(length(bad_cols))} Non-categorical
       column{?s} found: {.var {bad_cols}}.",
      call = call
    )
  }
  invisible(all_good)
}

check_column_factor <- function(data, column, call = caller_env()) {
  if (length(column) == 1 && !is.factor(data[[column]])) {
    cli::cli_abort("{.code {column}} should be a factor variable.", call = call)
  }
}

drop_unused_levels <- function(x) {
  if (is.factor(x)) droplevels(x) else x
}

warn_unused_levels <- function(data, column, call = caller_env()) {
  if (length(column) != 1) {
    return(invisible())
  }

  counts <- table(data[[column]])
  empty <- names(counts)[counts == 0]

  if (length(empty) > 0) {
    cli::cli_warn(
      c(
        "{cli::qty(empty)} Unused factor level{?s} {.val {empty}} in \\
         {.var {column}} {cli::qty(empty)}{?was/were} dropped.",
        i = "{cli::qty(empty)} Level{?s} with zero observations {?is/are} \\
             skipped when computing sampling targets."
      ),
      call = call
    )
  }

  invisible()
}

check_column_numeric <- function(data, column, call = caller_env()) {
  if (length(column) == 1 && !is.numeric(data[[column]])) {
    cli::cli_abort(
      "{.code {column}} should be a numeric variable.",
      call = call
    )
  }
}

check_var <- function(var, df, call = caller_env()) {
  if (length(var) != 1) {
    cli::cli_abort(
      "Please select a single factor variable for {.arg var}.",
      call = call
    )
  }

  var <- rlang::arg_match(var, names(df), error_call = call)
  column <- df[[var]]

  if (!(is.factor(column) || is.character(column))) {
    cli::cli_abort(
      "{.var {var}} should refer to a factor or character column, 
      not {.obj_type_friendly {column}}.",
      call = call
    )
  }
}

add_indicator_column <- function(new_data, n_orig, indicator_column) {
  if (!is.null(indicator_column)) {
    new_data[[indicator_column]] <- c(
      rep(FALSE, n_orig),
      rep(TRUE, nrow(new_data) - n_orig)
    )
  }
  new_data
}

check_case_weights_not_supported <- function(data, call = caller_env()) {
  has_weights <- vapply(data, hardhat::is_case_weights, logical(1))
  if (any(has_weights)) {
    cols <- names(has_weights)[has_weights]
    cli::cli_abort(
      c(
        "This step does not support case weights.",
        i = "The case weights column{?s} {.var {cols}} must be removed \\
             before this step."
      ),
      call = call
    )
  }
  invisible()
}

na_splice <- function(new_data, synthetic_data, object, call = caller_env()) {
  check_case_weights_not_supported(new_data, call = call)

  non_predictor <- setdiff(names(new_data), c(object$column, object$predictors))

  if (length(non_predictor) == 0) {
    return(synthetic_data)
  }

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

check_distance_arg <- function(distance, call = caller_env()) {
  rlang::arg_match(
    distance,
    c("euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev"),
    error_call = call
  )
}

# Drop the query point from each row of a neighbor-index matrix. `idx` has one
# row per query point and `k + 1` columns (the self-match plus `k` neighbors).
# The self-match is normally in the first column, but with exact-duplicate
# coordinates it can appear in any column or be missing entirely. Remove the
# column whose index equals the query row; if self is not present, drop the
# farthest (last) neighbor instead. Returns a matrix with `k` columns.
drop_self_neighbor <- function(idx) {
  n <- nrow(idx)
  out <- matrix(0L, nrow = n, ncol = ncol(idx) - 1L)
  for (i in seq_len(n)) {
    row <- idx[i, ]
    pos <- which(row == i)[1]
    if (is.na(pos)) {
      pos <- length(row)
    }
    out[i, ] <- row[-pos]
  }
  out
}

# Metrics whose distance equals ordinary Euclidean distance after a per-row or
# linear transform of the data, so the neighbor search can run on the transformed
# coordinates. manhattan and chebyshev are not in this set and must be computed
# with `stats::dist()` directly.
metric_is_transformable <- function(distance) {
  distance %in% c("euclidean", "cosine", "mahalanobis")
}

# Transform `data` so that Euclidean distance on the result equals the requested
# metric. For mahalanobis the whitening transform is derived from `cov_data`
# (the reference set in cross-distance calls) so a query and reference set can
# share one transform. `check_singular` toggles the guard for a covariance that
# cannot be inverted (more predictors than observations). Cosine-distance
# consumers that need true magnitudes must still convert the resulting Euclidean
# distance `d` via `d^2 / 2`; only the coordinate transform lives here.
metric_transform <- function(
  data,
  distance,
  cov_data = data,
  check_singular = TRUE,
  call = caller_env()
) {
  if (distance == "cosine") {
    norms <- sqrt(rowSums(data^2))
    norms[norms == 0] <- 1
    return(data / norms)
  }
  if (distance == "mahalanobis") {
    if (check_singular && nrow(cov_data) <= ncol(cov_data)) {
      cli::cli_abort(
        c(
          "{.code distance = \"mahalanobis\"} requires more observations than predictors in each class.",
          i = "{nrow(cov_data)} observation{?s} {?was/were} found but {ncol(cov_data)} predictor{?s} {?is/are} present.",
          i = "Try a different {.arg distance} metric or reduce the number of predictors."
        ),
        call = call
      )
    }
    S <- stats::cov(cov_data)
    return(data %*% solve(chol(S)))
  }
  # euclidean: no transform needed
  data
}

nn_indices <- function(data, k, distance) {
  if (metric_is_transformable(distance)) {
    data <- metric_transform(data, distance)
    return(RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx)
  }
  dist_method <- switch(
    distance,
    "manhattan" = "manhattan",
    "chebyshev" = "maximum"
  )
  dist_mat <- as.matrix(stats::dist(data, method = dist_method))
  t(apply(dist_mat, 1, \(x) order(x)[seq_len(k + 1)]))
}

nn_dists_cross <- function(query, reference, k, distance) {
  if (metric_is_transformable(distance)) {
    query_t <- metric_transform(query, distance, cov_data = reference)
    reference_t <- metric_transform(reference, distance, cov_data = reference)
    d <- RANN::nn2(reference_t, query_t, k = k)$nn.dists
    if (distance == "cosine") {
      # RANN returns Euclidean distances between unit vectors, sqrt(2 - 2*cos).
      # Convert to cosine distance (1 - cos_sim = d^2 / 2) so magnitudes are
      # correct for consumers such as NearMiss that average per-neighbor values.
      return(d^2 / 2)
    }
    return(d)
  }
  dist_method <- switch(
    distance,
    "manhattan" = "manhattan",
    "chebyshev" = "maximum"
  )
  combined <- rbind(query, reference)
  d_full <- as.matrix(stats::dist(combined, method = dist_method))
  nq <- nrow(query)
  d_cross <- d_full[seq_len(nq), nq + seq_len(nrow(reference)), drop = FALSE]
  t(apply(d_cross, 1, \(x) sort(x)[seq_len(k)]))
}

nn_indices_cross <- function(query, reference, k, distance) {
  if (metric_is_transformable(distance)) {
    query_t <- metric_transform(query, distance, cov_data = reference)
    reference_t <- metric_transform(reference, distance, cov_data = reference)
    return(RANN::nn2(reference_t, query_t, k = k)$nn.idx)
  }
  dist_method <- switch(
    distance,
    "manhattan" = "manhattan",
    "chebyshev" = "maximum"
  )
  combined <- rbind(query, reference)
  d_full <- as.matrix(stats::dist(combined, method = dist_method))
  nq <- nrow(query)
  d_cross <- d_full[seq_len(nq), nq + seq_len(nrow(reference)), drop = FALSE]
  t(apply(d_cross, 1, \(x) order(x)[seq_len(k)]))
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
