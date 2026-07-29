#' KMeans-SMOTE Algorithm
#'
#' KMeans-SMOTE clusters the predictor space, keeps only the clusters that are
#' dominated by the class being over-sampled, and generates new examples with
#' SMOTE inside those clusters, giving sparser clusters more of the new points.
#'
#' @inheritParams step_kmeans_smote
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-kmeans_smote
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Douzas, G., Bacao, F., and Last, F. (2018). Improving imbalanced
#'  learning through a heuristic oversampling method based on k-means and SMOTE.
#'  Information Sciences, 465:1-20.
#'
#' @seealso [step_kmeans_smote()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- kmeans_smote(circle_numeric, var = "class")
#'
#' res <- kmeans_smote(circle_numeric, var = "class", num_clusters = 10)
#'
#' res <- kmeans_smote(circle_numeric, var = "class", over_ratio = 0.8)
kmeans_smote <- function(
  df,
  var,
  k = 2,
  over_ratio = 1,
  num_clusters = NULL,
  cluster_balance_threshold = 1,
  density_exponent = NULL,
  distance = "euclidean"
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio, min = 0)
  check_number_whole(num_clusters, min = 2, allow_null = TRUE)
  check_number_decimal(
    cluster_balance_threshold,
    min = 0,
    allow_infinite = FALSE
  )
  check_number_decimal(density_exponent, min = 0, allow_null = TRUE)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  kmeans_smote_impl(
    df,
    var,
    k = k,
    over_ratio = over_ratio,
    num_clusters = num_clusters,
    cluster_balance_threshold = cluster_balance_threshold,
    density_exponent = density_exponent,
    distance = distance
  )
}

kmeans_smote_impl <- function(
  df,
  var,
  k,
  over_ratio,
  num_clusters = NULL,
  cluster_balance_threshold = 1,
  density_exponent = NULL,
  distance = "euclidean",
  call = caller_env()
) {
  df[[var]] <- as.factor(df[[var]])
  predictors <- setdiff(names(df), var)
  counts <- table(drop_unused_levels(df[[var]]))
  majority_count <- max(counts)
  ratio_target <- round(majority_count * over_ratio)
  which_upsample <- which(counts < ratio_target)
  samples_needed <- ratio_target - counts[which_upsample]
  min_names <- names(samples_needed)

  if (length(samples_needed) == 0) {
    return(df)
  }

  all_predictors <- as.matrix(df[, predictors, drop = FALSE])
  num_clusters <- num_clusters %||% default_num_clusters(nrow(df))
  exponent <- density_exponent %||% length(predictors)
  clusters <- kmeans_clusters(all_predictors, num_clusters, call = call)

  out_dfs <- list()

  for (i in seq_along(samples_needed)) {
    is_target <- df[[var]] == min_names[i]

    kept <- kmeans_smote_clusters(
      all_predictors,
      is_target,
      clusters,
      num_clusters = num_clusters,
      k = k,
      cluster_balance_threshold = cluster_balance_threshold,
      distance = distance
    )

    if (length(kept) == 0) {
      cli::cli_abort(
        c(
          "No cluster is suitable for over-sampling the minority class \\
           {.val {min_names[i]}}.",
          i = "No cluster both reached \\
               {.code cluster_balance_threshold = {cluster_balance_threshold}} \\
               and contained more than {k} observation{?s} of that class.",
          i = "Try a smaller {.arg cluster_balance_threshold}, a smaller \\
               {.arg num_clusters}, or fewer {.arg neighbors}."
        ),
        call = call
      )
    }

    quotas <- allocate_quotas(
      as.integer(samples_needed[i]),
      cluster_sparsity_weights(kept, exponent)
    )

    synthetic <- list()
    for (j in seq_along(kept)) {
      if (quotas[j] == 0) {
        next
      }
      synthetic[[length(synthetic) + 1L]] <- smote_data(
        kept[[j]]$data,
        k = k,
        n_samples = quotas[j],
        distance = distance
      )
    }

    out_df <- as.data.frame(do.call(rbind, synthetic))
    names(out_df) <- predictors
    out_df[var] <- df[[var]][which(is_target)[1]]
    out_dfs[[i]] <- out_df[names(df)]
  }

  final <- rbind(df, do.call(rbind, out_dfs))
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

# Collect the clusters that are eligible for over-sampling `is_target`, together
# with the information needed to weight them. A cluster is kept when the target
# class is dominant enough in it and it holds enough target observations for
# SMOTE interpolation.
kmeans_smote_clusters <- function(
  all_predictors,
  is_target,
  clusters,
  num_clusters,
  k,
  cluster_balance_threshold,
  distance
) {
  kept <- list()

  for (cluster in seq_len(num_clusters)) {
    in_cluster <- clusters == cluster
    n_target <- sum(in_cluster & is_target)
    n_other <- sum(in_cluster & !is_target)

    if (n_target <= k) {
      next
    }

    # A cluster made up entirely of the target class is maximally balanced
    # toward it, so it is always kept.
    balance <- if (n_other == 0) Inf else n_target / n_other
    if (balance < cluster_balance_threshold) {
      next
    }

    data <- all_predictors[in_cluster & is_target, , drop = FALSE]
    kept[[length(kept) + 1L]] <- list(
      data = data,
      n = n_target,
      avg_dist = mean_pairwise_dist(data, distance)
    )
  }

  kept
}

# Share of the new points each kept cluster should receive. Sparsity is
# `mean_pairwise_distance ^ exponent / n`, the reciprocal of the density used in
# the paper, so sparse clusters get more points. When every kept cluster has zero
# average distance (all of its points are identical) the sparsities are all zero
# and the ratio would be undefined, so the fallback is to weight by cluster size.
cluster_sparsity_weights <- function(kept, exponent) {
  avg_dist <- vapply(kept, \(x) x$avg_dist, numeric(1))
  n <- vapply(kept, \(x) x$n, numeric(1))

  sparsity <- avg_dist^exponent / n

  if (!all(is.finite(sparsity)) || sum(sparsity) == 0) {
    return(n / sum(n))
  }

  sparsity / sum(sparsity)
}

# Split `n` points over `weights` with the largest remainder method. Rounding
# each quota up individually would generate more points than were asked for, so
# the quotas are floored and the leftovers handed to the largest remainders. The
# result always sums to exactly `n`.
allocate_quotas <- function(n, weights) {
  raw <- n * weights
  out <- floor(raw)
  short <- n - sum(out)

  if (short > 0) {
    largest <- order(raw - out, decreasing = TRUE)[seq_len(short)]
    out[largest] <- out[largest] + 1
  }

  out
}

# Mean distance between all pairs of rows, used to measure how spread out a
# cluster is. Both branches mirror how `nn_indices()` computes distances for the
# metric, so the sparsity weighting respects `distance`.
mean_pairwise_dist <- function(data, distance) {
  if (nrow(data) < 2) {
    return(0)
  }

  if (metric_is_transformable(distance)) {
    dists <- stats::dist(metric_transform(data, distance))
    return(mean(metric_rescale_dists(as.numeric(dists), distance)))
  }

  dists <- dense_dist_matrix(data, distance)
  mean(dists[lower.tri(dists)])
}

# Rule of thumb for how many clusters to use when the user gives none. Grows with
# the data but stays small enough that clusters keep enough observations to
# interpolate within.
default_num_clusters <- function(n_rows) {
  max(2, floor(sqrt(n_rows / 2)))
}

kmeans_clusters <- function(data, n_clusters, call = caller_env()) {
  n_distinct_rows <- nrow(unique(data))

  if (n_distinct_rows < n_clusters) {
    cli::cli_abort(
      c(
        "Not enough distinct observations to compute {n_clusters} cluster{?s}.",
        i = "{n_distinct_rows} distinct observation{?s} {?was/were} found.",
        i = "Try a smaller {.arg num_clusters} or remove duplicated rows."
      ),
      call = call
    )
  }

  kmeans_fit(data, n_clusters)$cluster
}
