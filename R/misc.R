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

# `over_ratio` and `under_ratio` accept either a single number (applied to every
# class) or a named numeric vector giving per-class ratios. This only validates
# the shape of the argument; names are checked against the observed levels by
# check_ratio_levels() once the data is known.
check_ratio <- function(ratio, arg = caller_arg(ratio), call = caller_env()) {
  if (length(ratio) == 1 && is.null(names(ratio))) {
    check_number_decimal(ratio, arg = arg, min = 0, call = call)
    return(invisible(NULL))
  }

  if (!is.numeric(ratio) || length(ratio) == 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number or a named numeric vector, \\
       not {.obj_type_friendly {ratio}}.",
      call = call
    )
  }

  nms <- names(ratio)
  if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number or a named numeric vector.",
        i = "Every element must be named with a level of the outcome."
      ),
      call = call
    )
  }

  dupes <- unique(nms[duplicated(nms)])
  if (length(dupes) > 0) {
    cli::cli_abort(
      "{.arg {arg}} must have unique names, \\
       but {.val {dupes}} {?is/are} duplicated.",
      call = call
    )
  }

  if (!all(is.finite(ratio))) {
    cli::cli_abort(
      "{.arg {arg}} must be finite, not missing or infinite.",
      call = call
    )
  }

  if (any(ratio < 0)) {
    cli::cli_abort(
      "{.arg {arg}} must be larger than or equal to 0.",
      call = call
    )
  }

  invisible(NULL)
}

check_ratio_levels <- function(
  ratio,
  levels,
  arg = caller_arg(ratio),
  call = caller_env()
) {
  if (is.null(names(ratio))) {
    return(invisible(NULL))
  }

  unknown <- setdiff(names(ratio), levels)
  if (length(unknown) > 0) {
    if (length(levels) == 0) {
      available <- "No levels were observed in the outcome."
    } else {
      available <- "Available {cli::qty(levels)}level{?s}: {.val {levels}}."
    }
    cli::cli_abort(
      c(
        "{.arg {arg}} names must be levels of the outcome.",
        x = "Unknown {cli::qty(unknown)}name{?s}: {.val {unknown}}.",
        i = available
      ),
      call = call
    )
  }

  invisible(NULL)
}

# Same check, done at prep() time against the outcome column, so a typo fails
# fast instead of surfacing from the implementation during bake().
check_ratio_column <- function(
  ratio,
  data,
  column,
  arg = caller_arg(ratio),
  call = caller_env()
) {
  if (length(column) != 1 || is.null(names(ratio))) {
    return(invisible(NULL))
  }

  levels <- levels(drop_unused_levels(as.factor(data[[column]])))
  check_ratio_levels(ratio, levels, arg = arg, call = call)
}

# Per-class target counts, aligned to and named like `counts`. Classes not named
# in `ratio` keep their current count, which leaves them untouched downstream.
ratio_target <- function(
  counts,
  ratio,
  reference,
  arg = caller_arg(ratio),
  call = caller_env()
) {
  target <- stats::setNames(as.numeric(counts), names(counts))

  check_ratio_levels(ratio, names(target), arg = arg, call = call)

  if (length(target) == 0) {
    return(target)
  }

  ref <- reference(counts)

  if (is.null(names(ratio))) {
    target[] <- ref * ratio
    return(target)
  }

  # Index by position. Assigning by name appends for a name that isn't present,
  # which would leave `target` longer than `counts` and silently recycle.
  target[match(names(ratio), names(target))] <- ref * ratio
  target
}

over_target <- function(
  counts,
  over_ratio,
  arg = "over_ratio",
  call = caller_env()
) {
  ratio_target(counts, over_ratio, max, arg = arg, call = call)
}

under_target <- function(
  counts,
  under_ratio,
  arg = "under_ratio",
  call = caller_env()
) {
  ratio_target(counts, under_ratio, min, arg = arg, call = call)
}

# ROSE scales the size of the whole generated sample rather than setting a
# target per class, so per-class ratios have no meaning there.
check_scalar_ratio <- function(
  ratio,
  arg = caller_arg(ratio),
  call = caller_env()
) {
  if (!is.null(names(ratio))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number, not a named vector.",
        i = "Per-class ratios are not supported here because {.arg {arg}} \\
             scales the size of the total generated sample."
      ),
      call = call
    )
  }
  check_number_decimal(ratio, arg = arg, min = 0, call = call)
}

# Target for a single class at bake() time. Levels that were not present in the
# training data have no target and are left untouched, which `untouched` encodes
# as the count that makes the sampler a no-op (0 when up-sampling, `Inf` when
# down-sampling).
class_target <- function(target, name, untouched) {
  if (length(name) == 1 && !is.na(name) && name %in% names(target)) {
    target[[name]]
  } else {
    untouched
  }
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
    c(
      "euclidean",
      "cosine",
      "mahalanobis",
      "manhattan",
      "chebyshev",
      sqrt_embedded_metrics(),
      philentropy_metrics()
    ),
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

# Probability-divergence metrics that equal Euclidean distance on the elementwise
# square root of the data, either exactly or after a monotone transform of the
# resulting distance (see `metric_rescale_dists()`). Because the transform is
# monotone, a RANN search on the square-rooted coordinates returns exactly the
# right neighbor *indices*; only distance magnitudes need converting.
#
# squared_chord and matusita hold for any non-negative rows. hellinger and
# bhattacharyya are derived from the fidelity `1 - d^2 / 2`, an identity that
# only holds when each row sums to one, so they additionally require rows to be
# probability distributions.
sqrt_embedded_metrics <- function() {
  c("squared_chord", "matusita", "hellinger", "bhattacharyya")
}

simplex_metrics <- function() {
  c("hellinger", "bhattacharyya")
}

# Metrics whose distance equals ordinary Euclidean distance after a per-row or
# linear transform of the data, so the neighbor search can run on the transformed
# coordinates via RANN. manhattan and chebyshev are not in this set: RANN only
# supports Euclidean distance and there is no distance-preserving transform for
# them, so they fall back to a dense `stats::dist()` matrix. Replacing that with a
# lighter k-NN search would require either a new dependency or a hand-rolled
# routine whose tie-breaking could differ from `order()`/`sort()` and change
# results, so the dense path is kept for these two metrics.
metric_is_transformable <- function(distance) {
  distance %in%
    c("euclidean", "cosine", "mahalanobis", sqrt_embedded_metrics())
}

# Divergence measures computed by philentropy. Deliberately a curated allowlist
# rather than `philentropy::getDistMethods()`:
#
# * That list mixes distances with similarity measures (intersection, cosine,
#   fidelity, inner_product, harmonic_mean, hassebrook, kulczynski_s, ruzicka),
#   where a *larger* value means a *closer* pair. The neighbor search sorts
#   ascending, so those would silently return the farthest neighbors.
# * philentropy computes one triangle of the distance matrix and mirrors it, so
#   asymmetric measures such as kullback-leibler come back symmetrized rather
#   than as the requested divergence. Only symmetric measures are listed here.
# * Names already handled by a faster path (euclidean, manhattan, chebyshev,
#   cosine, and the sqrt-embedded metrics) are excluded so each metric has
#   exactly one spelling and one backend.
philentropy_metrics <- function() {
  c(
    "canberra",
    "soergel",
    "lorentzian",
    "jeffreys",
    "topsoe",
    "jensen-shannon",
    "jensen_difference",
    "taneja",
    "kumar-johnson"
  )
}

# philentropy metrics that divide by, or take the log of, individual values, so a
# zero anywhere makes the true distance infinite. philentropy returns a finite
# but meaningless number in that case rather than `Inf`, so the input has to be
# checked up front; there is no non-finite result to detect afterwards.
philentropy_positive_metrics <- function() {
  c("jeffreys", "taneja", "kumar-johnson")
}

# Which engine computes distances for `distance`. "rann" is the fast approximate
# path, "dist" and "philentropy" both build a dense O(n^2) matrix.
metric_backend <- function(distance) {
  if (metric_is_transformable(distance)) {
    "rann"
  } else if (distance %in% philentropy_metrics()) {
    "philentropy"
  } else {
    "dist"
  }
}

check_metric_nonnegative <- function(data, distance, call = caller_env()) {
  if (any(data < 0)) {
    cli::cli_abort(
      c(
        "{.code distance = \"{distance}\"} requires non-negative predictor values.",
        i = "Negative values were found in the columns used to compute distances.",
        i = "Each row is treated as a probability distribution by this metric.",
        i = "Try a different {.arg distance} metric or rescale the predictors."
      ),
      call = call
    )
  }
  invisible()
}

check_metric_positive <- function(data, distance, call = caller_env()) {
  if (any(data <= 0)) {
    cli::cli_abort(
      c(
        "{.code distance = \"{distance}\"} requires strictly positive predictor values.",
        i = "Zero or negative values were found in the columns used to compute \\
             distances.",
        i = "This metric divides by individual values, so a zero makes the \\
             distance infinite.",
        i = "Try {.code distance = \"jensen-shannon\"} or \\
             {.code distance = \"canberra\"}, which allow zeros."
      ),
      call = call
    )
  }
  invisible()
}

check_metric_simplex <- function(data, distance, call = caller_env()) {
  bad <- abs(rowSums(data) - 1) > 1e-6
  if (any(bad)) {
    cli::cli_abort(
      c(
        "{.code distance = \"{distance}\"} requires each row to sum to 1.",
        i = "{sum(bad)} row{?s} do{?es/} not sum to 1.",
        i = "Each row is treated as a probability distribution by this metric.",
        i = "Try {.code distance = \"matusita\"} or \\
             {.code distance = \"squared_chord\"}, which do not require this."
      ),
      call = call
    )
  }
  invisible()
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
  if (distance %in% sqrt_embedded_metrics()) {
    check_metric_nonnegative(data, distance, call = call)
    if (distance %in% simplex_metrics()) {
      check_metric_simplex(data, distance, call = call)
    }
    return(sqrt(data))
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
    return(data %*% solve(chol_cov(S, call = call)))
  }
  # euclidean: no transform needed
  data
}

# Cholesky factor of a covariance matrix, replacing the raw "the leading minor
# of order k is not positive definite" error from `chol()` with a message that
# points at the `distance` argument. A covariance matrix is singular when
# predictors are collinear (including constant columns) or when duplicate rows
# leave fewer distinct observations than predictors.
#
# Exactly collinear predictors do not always trip `chol()`: rounding can leave
# the offending pivot a tiny positive number instead of zero, in which case
# `chol()` succeeds and `solve()` returns an inverse of astronomical magnitude,
# silently poisoning every distance. A rank check on the covariance catches those
# cases before the factorization, using the same relative tolerance as `lm()`.
chol_cov <- function(S, call = caller_env()) {
  singular <- function(cnd = NULL) {
    cli::cli_abort(
      c(
        "{.code distance = \"mahalanobis\"} requires an invertible covariance
         matrix, but the covariance of the predictors is singular.",
        i = "This happens when predictors are collinear or constant, or when
             duplicated rows leave too few distinct observations.",
        i = "Try a different {.arg distance} metric or remove the redundant
             predictors."
      ),
      call = call,
      parent = cnd
    )
  }

  if (qr(S)$rank < ncol(S)) {
    singular()
  }

  rlang::try_fetch(chol(S), error = function(cnd) singular(cnd))
}

# Convert Euclidean distances computed on `metric_transform()`ed coordinates into
# the magnitudes of the requested metric. Each conversion is monotone increasing
# in `d`, so this never reorders neighbors; it only matters for consumers such as
# NearMiss that average per-neighbor distance values. Metrics whose transform is
# already distance-preserving (euclidean, mahalanobis, matusita) fall through.
metric_rescale_dists <- function(d, distance) {
  switch(
    distance,
    # RANN returns Euclidean distances between unit vectors, sqrt(2 - 2*cos).
    # Cosine distance is 1 - cos_sim = d^2 / 2.
    "cosine" = d^2 / 2,
    "squared_chord" = d^2,
    "hellinger" = sqrt(2) * d,
    # -log(fidelity), where fidelity = 1 - d^2 / 2. Rows with disjoint support
    # give fidelity 0 and an infinite distance, which is correct; the `pmax()`
    # only keeps floating-point error from pushing fidelity below 0 into NaN.
    "bhattacharyya" = -log(pmax(1 - d^2 / 2, 0)),
    d
  )
}

# Dense all-pairs distance matrix for the metrics that have no fast path. Both
# backends here are O(n^2) in time and memory.
dense_dist_matrix <- function(data, distance, call = caller_env()) {
  if (metric_backend(distance) == "philentropy") {
    rlang::check_installed(
      "philentropy",
      sprintf("for `distance = \"%s\"`.", distance),
      call = call
    )
    check_metric_nonnegative(data, distance, call = call)
    if (distance %in% philentropy_positive_metrics()) {
      check_metric_positive(data, distance, call = call)
    }
    # `mute.message` silences some but not all of philentropy's chatter, so the
    # remaining messages are suppressed here to keep step output clean.
    res <- suppressMessages(philentropy::distance(
      as.matrix(data),
      method = distance,
      test.na = FALSE,
      mute.message = TRUE
    ))
    # With exactly two rows philentropy returns a bare scalar rather than a
    # 2x2 matrix, so the callers' `apply()` over rows would fail.
    if (!is.matrix(res)) {
      res <- rbind(c(0, res), c(res, 0))
    }
    return(unname(res))
  }
  dist_method <- switch(
    distance,
    "manhattan" = "manhattan",
    "chebyshev" = "maximum"
  )
  as.matrix(stats::dist(data, method = dist_method))
}

# Dense query-by-reference distance block for the metrics with no fast path.
# Neither backend computes a rectangular block directly, so both sets are stacked
# and the query-vs-reference corner is cut out of the full matrix.
dense_dist_cross <- function(query, reference, distance, call = caller_env()) {
  d_full <- dense_dist_matrix(rbind(query, reference), distance, call = call)
  nq <- nrow(query)
  d_full[seq_len(nq), nq + seq_len(nrow(reference)), drop = FALSE]
}

nn_indices <- function(data, k, distance) {
  if (metric_is_transformable(distance)) {
    data <- metric_transform(data, distance)
    return(RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx)
  }
  dist_mat <- dense_dist_matrix(data, distance)
  t(apply(dist_mat, 1, \(x) order(x)[seq_len(k + 1)]))
}

nn_dists_cross <- function(query, reference, k, distance) {
  if (metric_is_transformable(distance)) {
    query_t <- metric_transform(query, distance, cov_data = reference)
    reference_t <- metric_transform(reference, distance, cov_data = reference)
    d <- RANN::nn2(reference_t, query_t, k = k)$nn.dists
    return(metric_rescale_dists(d, distance))
  }
  d_cross <- dense_dist_cross(query, reference, distance)
  res <- apply(d_cross, 1, \(x) sort(x)[seq_len(k)])
  matrix(res, nrow = nrow(query), ncol = k, byrow = TRUE)
}

nn_indices_cross <- function(query, reference, k, distance) {
  if (metric_is_transformable(distance)) {
    query_t <- metric_transform(query, distance, cov_data = reference)
    reference_t <- metric_transform(reference, distance, cov_data = reference)
    return(RANN::nn2(reference_t, query_t, k = k)$nn.idx)
  }
  d_cross <- dense_dist_cross(query, reference, distance)
  res <- apply(d_cross, 1, \(x) order(x)[seq_len(k)])
  matrix(res, nrow = nrow(query), ncol = k, byrow = TRUE)
}

# Shared condensation scan for CNN and one-sided selection. Walks `candidates`
# in order and, using a 1-nearest-neighbor rule against the current store, adds
# any candidate whose nearest stored neighbor has a different class. The store is
# unchanged between additions, so the cross-NN is computed in one batch for all
# not-yet-processed candidates and only recomputed after the store actually
# changes, rather than rebuilding the neighbor search for every single candidate.
# Results are identical to the per-candidate scan. Returns the updated `in_store`
# and whether anything was added.
condense_scan <- function(candidates, in_store, predictors, outcome, distance) {
  added <- FALSE
  batch <- NULL
  store_idx <- NULL
  base <- 0L
  dirty <- TRUE
  n <- length(candidates)
  for (idx in seq_len(n)) {
    i <- candidates[idx]
    if (dirty) {
      store_idx <- which(in_store)
      remaining <- candidates[idx:n]
      batch <- nn_indices_cross(
        predictors[remaining, , drop = FALSE],
        predictors[store_idx, , drop = FALSE],
        k = 1,
        distance = distance
      )
      base <- idx - 1L
      dirty <- FALSE
    }
    nn1 <- batch[idx - base, 1]
    if (outcome[store_idx[nn1]] != outcome[i]) {
      in_store[i] <- TRUE
      added <- TRUE
      dirty <- TRUE
    }
  }
  list(in_store = in_store, added = added)
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
