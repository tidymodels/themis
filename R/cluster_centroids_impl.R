#' ClusterCentroids Algorithm
#'
#' Under-samples the majority classes by replacing them with cluster
#' representatives found with k-means.
#'
#' @inheritParams step_cluster_centroids
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-cluster_centroids
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @seealso [step_cluster_centroids()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- cluster_centroids(circle_numeric, var = "class")
#'
#' res <- cluster_centroids(circle_numeric, var = "class", voting = "hard")
#'
#' res <- cluster_centroids(circle_numeric, var = "class", under_ratio = 1.5)
cluster_centroids <- function(
  df,
  var,
  under_ratio = 1,
  voting = "soft"
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_decimal(under_ratio, min = 0)
  voting <- rlang::arg_match(voting, c("soft", "hard"))

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  cluster_centroids_impl(
    df,
    var,
    ignore_vars = character(),
    under_ratio = under_ratio,
    voting = voting
  )
}

cluster_centroids_impl <- function(
  df,
  var,
  ignore_vars = character(),
  under_ratio = 1,
  voting = "soft",
  call = caller_env()
) {
  predictors <- setdiff(names(df), c(var, ignore_vars))
  counts <- table(drop_unused_levels(df[[var]]))
  n_target <- max(1, floor(min(counts) * under_ratio))
  under_classes <- names(counts)[counts > n_target]

  if (length(under_classes) == 0) {
    return(df)
  }

  keep_rows <- rep(TRUE, nrow(df))
  out_dfs <- list()

  for (class in under_classes) {
    rows <- which(df[[var]] == class)
    class_data <- as.matrix(df[rows, predictors, drop = FALSE])
    centers <- kmeans_centers(class_data, n_target, class, call = call)

    if (voting == "hard") {
      # The representative must come from the class being under-sampled, so the
      # neighbor search only ever sees that class's observations.
      selected <- unique(RANN::nn2(class_data, centers, k = 1)$nn.idx[, 1])
      keep_rows[setdiff(rows, rows[selected])] <- FALSE
    } else {
      keep_rows[rows] <- FALSE
      out_dfs[[class]] <- centroid_rows(centers, df, var, predictors, rows[1])
    }
  }

  final <- rbind(df[keep_rows, ], do.call(rbind, out_dfs))
  if (is.factor(df[[var]])) {
    final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  }
  rownames(final) <- NULL
  final
}

# Turn a matrix of centroids into rows shaped like `df`. Columns that are
# neither the outcome nor a predictor have no centroid value, so they are filled
# with a typed missing value, matching how the synthesizing steps treat
# non-predictor columns.
centroid_rows <- function(centers, df, var, predictors, class_row) {
  out_df <- as.data.frame(centers)
  names(out_df) <- predictors
  out_df[[var]] <- df[[var]][class_row]

  for (col in setdiff(names(df), c(predictors, var))) {
    out_df[[col]] <- df[[col]][rep(NA_integer_, nrow(out_df))]
  }

  out_df[names(df)]
}

kmeans_centers <- function(data, n_clusters, class, call = caller_env()) {
  n_distinct_rows <- nrow(unique(data))

  if (n_distinct_rows < n_clusters) {
    cli::cli_abort(
      c(
        "Not enough distinct observations in {.val {class}} to compute \\
         {n_clusters} cluster{?s}.",
        i = "{n_distinct_rows} distinct observation{?s} {?was/were} found.",
        i = "Try a smaller {.arg under_ratio} or remove duplicated rows."
      ),
      call = call
    )
  }

  # Hartigan-Wong reports a warning when it hits an internal iteration limit but
  # still returns usable centers, so those warnings are muffled rather than
  # passed on to the user.
  res <- withCallingHandlers(
    stats::kmeans(data, centers = n_clusters, iter.max = 100),
    warning = function(cnd) {
      if (grepl("converge|Quick-TRANSfer", conditionMessage(cnd))) {
        rlang::cnd_muffle(cnd)
      }
    }
  )

  res$centers
}
