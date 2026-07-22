#' SMOGN Algorithm
#'
#' SMOGN generates new examples for imbalanced regression problems. It
#' identifies rare regions of the continuous outcome using a relevance
#' function, over-samples those regions with a combination of SMOTE-style
#' interpolation and the introduction of Gaussian noise, and under-samples the
#' common regions.
#'
#' @inheritParams step_smogn
#' @param df data.frame or tibble. Must have 1 numeric outcome variable and
#'  remaining numeric variables.
#' @param var Character, name of the numeric outcome variable.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-smogn
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Branco, P., Torgo, L., and Ribeiro, R. P. (2017). SMOGN: a
#'  pre-processing approach for imbalanced regression. Proceedings of Machine
#'  Learning Research, 74:36-50.
#'
#' @seealso [step_smogn()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y")]
#'
#' res <- smogn(circle_numeric, var = "x")
#'
#' res <- smogn(circle_numeric, var = "x", neighbors = 10)
smogn <- function(
  df,
  var,
  threshold = 0.5,
  relevance = NULL,
  neighbors = 5,
  perturbation = 0.02,
  distance = "euclidean"
) {
  check_data_frame(df)
  check_number_whole(neighbors, min = 1)
  check_number_decimal(threshold, min = 0, max = 1)
  check_number_decimal(perturbation, min = 0)
  check_distance_arg(distance)

  if (length(var) != 1 || !var %in% names(df)) {
    cli::cli_abort("Please select a single numeric variable for {.arg var}.")
  }
  if (!is.numeric(df[[var]])) {
    cli::cli_abort("{.var {var}} should refer to a numeric column.")
  }

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors, drop = FALSE])
  check_na(df)

  res <- smogn_impl(
    df,
    var,
    ignore_vars = character(),
    threshold = threshold,
    relevance = relevance,
    k = neighbors,
    perturbation = perturbation,
    distance = distance
  )
  attr(res, "n_synthetic") <- NULL
  res
}

smogn_impl <- function(
  df,
  var,
  ignore_vars = character(),
  threshold = 0.5,
  relevance = NULL,
  k = 5,
  perturbation = 0.02,
  distance = "euclidean",
  call = caller_env()
) {
  y <- df[[var]]
  predictors <- setdiff(names(df), c(var, ignore_vars))

  phi <- smogn_relevance(y, relevance, call = call)
  rare <- phi >= threshold

  if (!any(rare)) {
    cli::cli_abort(
      c(
        "No rare values were found in {.arg {var}} with the current {.arg threshold}.",
        i = "Lower {.arg threshold} or supply relevance control points via {.arg relevance}."
      ),
      call = call
    )
  }

  # Split into contiguous bins of the outcome that share rare/common status
  ord <- order(y)
  rare_ord <- rare[ord]
  bin_ord <- cumsum(c(
    TRUE,
    rare_ord[-1] != rare_ord[-length(rare_ord)]
  ))
  bins <- integer(length(y))
  bins[ord] <- bin_ord

  n_bins <- max(bins)
  target <- round(length(y) / n_bins)

  keep_dfs <- list()
  syn_dfs <- list()

  for (b in seq_len(n_bins)) {
    idx <- which(bins == b)
    n_i <- length(idx)
    bin_df <- df[idx, , drop = FALSE]

    if (n_i >= target) {
      keep_dfs[[b]] <- df[idx[sample.int(length(idx), target)], , drop = FALSE]
      next
    }

    keep_dfs[[b]] <- bin_df
    n_gen <- target - n_i

    if (n_i < 2) {
      next
    }

    syn <- smogn_generate(
      x = as.matrix(bin_df[, predictors, drop = FALSE]),
      y = bin_df[[var]],
      n_generate = n_gen,
      k = k,
      perturbation = perturbation,
      distance = distance
    )

    syn_df <- bin_df[rep(1L, n_gen), , drop = FALSE]
    for (j in seq_along(predictors)) {
      syn_df[[predictors[j]]] <- syn$x[, j]
    }
    syn_df[[var]] <- syn$y
    for (col in ignore_vars) {
      syn_df[[col]][] <- NA
    }
    syn_dfs[[length(syn_dfs) + 1L]] <- syn_df
  }

  kept <- do.call(rbind, keep_dfs)
  synth <- if (length(syn_dfs) > 0) do.call(rbind, syn_dfs) else NULL

  final <- rbind(kept, synth)
  rownames(final) <- NULL
  final <- final[, names(df), drop = FALSE]
  attr(final, "n_synthetic") <- if (is.null(synth)) 0L else nrow(synth)
  final
}

# Compute a relevance value in [0, 1] for each outcome value. When `relevance`
# is `NULL`, control points are derived automatically from the boxplot extremes
# of `y` (median has relevance 0, the whisker extremes have relevance 1) and a
# monotone piecewise-cubic interpolation is used between them.
smogn_relevance <- function(y, relevance = NULL, call = caller_env()) {
  if (is.null(relevance)) {
    finite_y <- y[is.finite(y)]
    if (length(unique(finite_y)) < 3 || stats::IQR(finite_y) == 0) {
      cli::cli_abort(
        c(
          "Unable to determine rare values automatically for the outcome.",
          i = "The outcome distribution is degenerate (zero \\
          interquartile range or heavily tied values), so relevance \\
          control points cannot be derived from its boxplot extremes.",
          i = "Supply relevance control points via the {.arg relevance} argument."
        ),
        call = call
      )
    }
    stats <- grDevices::boxplot.stats(y)$stats
    med <- stats[3]
    cx <- med
    cy <- 0
    if (stats[1] < med) {
      cx <- c(stats[1], cx)
      cy <- c(1, cy)
    }
    if (stats[5] > med) {
      cx <- c(cx, stats[5])
      cy <- c(cy, 1)
    }
    if (length(cx) < 2) {
      cli::cli_abort(
        c(
          "Unable to determine rare values automatically for the outcome.",
          i = "Supply relevance control points via the {.arg relevance} argument."
        ),
        call = call
      )
    }
  } else {
    if (!is.matrix(relevance) || ncol(relevance) < 2) {
      cli::cli_abort(
        "{.arg relevance} must be a matrix with at least two columns \\
        (outcome value and relevance).",
        call = call
      )
    }
    o <- order(relevance[, 1])
    cx <- relevance[o, 1]
    cy <- relevance[o, 2]
  }

  fn <- stats::splinefun(cx, cy, method = "monoH.FC")
  pmin(pmax(fn(y), 0), 1)
}

# Distance matrix within a bin, honoring the chosen metric by transforming the
# data (cosine, mahalanobis) or using the appropriate `stats::dist` method.
smogn_distmat <- function(data, distance) {
  if (distance == "cosine") {
    norms <- sqrt(rowSums(data^2))
    norms[norms == 0] <- 1
    # Euclidean distance between unit vectors is sqrt(2 - 2*cos); convert to
    # cosine distance (1 - cos_sim = d^2 / 2) so the interpolate-vs-noise test
    # compares true cosine-distance magnitudes.
    d <- as.matrix(stats::dist(data / norms))
    return(d^2 / 2)
  }
  if (distance == "mahalanobis") {
    S <- stats::cov(data)
    data <- data %*% solve(chol(S))
    return(as.matrix(stats::dist(data)))
  }
  method <- switch(
    distance,
    "euclidean" = "euclidean",
    "manhattan" = "manhattan",
    "chebyshev" = "maximum"
  )
  as.matrix(stats::dist(data, method = method))
}

smogn_generate <- function(
  x,
  y,
  n_generate,
  k,
  perturbation,
  distance
) {
  n <- nrow(x)
  k <- min(k, n - 1)
  dmat <- smogn_distmat(x, distance)

  sd_x <- apply(x, 2, stats::sd)
  sd_x[is.na(sd_x)] <- 0
  sd_y <- stats::sd(y)
  if (is.na(sd_y)) {
    sd_y <- 0
  }

  new_x <- matrix(0, nrow = n_generate, ncol = ncol(x))
  new_y <- numeric(n_generate)
  seeds <- rep(sample(n), length.out = n_generate)

  for (i in seq_len(n_generate)) {
    s <- seeds[i]
    ord <- order(dmat[s, ])
    knn <- ord[ord != s][seq_len(k)]
    maxD <- 0.5 * stats::median(dmat[s, knn])
    nb <- knn[sample.int(k, 1L)]
    d <- dmat[s, nb]

    if (d < maxD) {
      # Safe: SMOTE-style interpolation between the seed and its neighbor
      u <- stats::runif(1)
      new_x[i, ] <- x[s, ] + u * (x[nb, ] - x[s, ])
      new_y[i] <- y[s] + u * (y[nb] - y[s])
    } else {
      # Unsafe: perturb the seed with Gaussian noise
      new_x[i, ] <- x[s, ] +
        stats::rnorm(ncol(x), 0, sd_x * min(perturbation, maxD))
      new_y[i] <- y[s] +
        stats::rnorm(1, 0, sd_y * min(perturbation, maxD))
    }
  }

  list(x = new_x, y = new_y)
}
