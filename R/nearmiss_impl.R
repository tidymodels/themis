#' Remove Points Near Other Classes
#'
#' Generates synthetic positive instances using nearmiss algorithm.
#'
#' @inheritParams step_nearmiss
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-nearmiss
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Inderjeet Mani and I Zhang. knn approach to unbalanced data
#' distributions: a case study involving information extraction. In Proceedings
#' of workshop on learning from imbalanced datasets, 2003.
#'
#' @seealso [step_nearmiss()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- nearmiss(circle_numeric, var = "class")
#'
#' res <- nearmiss(circle_numeric, var = "class", k = 10)
#'
#' res <- nearmiss(circle_numeric, var = "class", under_ratio = 1.5)
#'
#' res <- nearmiss(circle_numeric, var = "class", distance = "manhattan")
#'
#' res <- nearmiss(circle_numeric, var = "class", version = 2)
#'
#' res <- nearmiss(circle_numeric, var = "class", version = 3)
#'
#' res <- nearmiss(
#'   circle_numeric,
#'   var = "class",
#'   version = 3,
#'   n_neighbors_ver3 = 10
#' )
nearmiss <- function(
  df,
  var,
  k = 5,
  under_ratio = 1,
  distance = "euclidean",
  version = 1,
  n_neighbors_ver3 = 3
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(under_ratio)
  check_distance_arg(distance)
  check_number_whole(version, min = 1, max = 3)
  check_number_whole(n_neighbors_ver3, min = 1)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  nearmiss_impl(
    df,
    var,
    ignore_vars = character(),
    k,
    under_ratio,
    distance = distance,
    version = version,
    n_neighbors_ver3 = n_neighbors_ver3
  )
}

nearmiss_impl <- function(
  df,
  var,
  ignore_vars,
  k = 5,
  under_ratio = 1,
  distance = "euclidean",
  version = 1,
  n_neighbors_ver3 = 3,
  call = caller_env()
) {
  classes <- downsample_count(df, var, under_ratio)

  deleted_rows <- integer()
  for (i in seq_along(classes)) {
    df_only <- df[, !names(df) %in% ignore_vars]
    class <- subset_to_matrix(df_only, var, names(classes)[i])
    not_class <- subset_to_matrix(df_only, var, names(classes)[i], FALSE)

    if (nrow(not_class) <= k) {
      cli::cli_abort(
        c(
          "Not enough observations in the other classes to compute {k} nearest neighbors for {.val {names(classes)[i]}}.",
          i = "{nrow(not_class)} observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
        ),
        call = call
      )
    }

    n_keep <- nrow(class) - classes[i]

    if (version == 1) {
      dists <- nn_dists_cross(class, not_class, k, distance)
      selected_ind <- rank(rowMeans(dists), ties.method = "first") <= n_keep
    } else if (version == 2) {
      # The k farthest neighbors instead of the k nearest ones. The distances
      # returned by `nn_dists_cross()` are sorted increasingly, so requesting
      # all of them and taking the last k columns gives the farthest ones.
      dists <- nn_dists_cross(class, not_class, nrow(not_class), distance)
      dists <- dists[, seq(ncol(dists) - k + 1, ncol(dists)), drop = FALSE]
      selected_ind <- rank(rowMeans(dists), ties.method = "first") <= n_keep
    } else {
      if (n_neighbors_ver3 > nrow(class)) {
        cli::cli_abort(
          c(
            "Not enough observations in {.val {names(classes)[i]}} to compute {n_neighbors_ver3} nearest neighbors for the NearMiss-3 candidate pool.",
            i = "{nrow(class)} observation{?s} {?was/were} found, but {n_neighbors_ver3} {?is/are} needed.",
            i = "Lower {.arg n_neighbors_ver3}."
          ),
          call = call
        )
      }

      # First stage: for each observation of the other classes, keep its
      # `n_neighbors_ver3` nearest neighbors within this class as candidates.
      # Everything outside of that pool is removed.
      pool_ind <- nn_indices_cross(
        not_class,
        class,
        n_neighbors_ver3,
        distance
      )
      pool <- sort(unique(as.vector(pool_ind)))

      # Second stage: within the pool, keep the observations that are the
      # farthest away from their k nearest neighbors in the other classes.
      dists <- nn_dists_cross(
        class[pool, , drop = FALSE],
        not_class,
        k,
        distance
      )
      selected_ind <- rep(FALSE, nrow(class))
      selected_ind[pool] <- rank(-rowMeans(dists), ties.method = "first") <=
        n_keep
    }

    deleted_rows <- c(
      deleted_rows,
      which(df[[var]] %in% names(classes)[i])[!selected_ind]
    )
  }

  if (length(deleted_rows) > 0) {
    df <- df[-deleted_rows, ]
  }
  df
}

downsample_count <- function(data, var, ratio) {
  counts <- table(drop_unused_levels(data[[var]]))
  min_count <- min(counts)
  ratio_target <- min_count * ratio
  which_class <- which(counts > ratio_target)
  counts[which_class] - ratio_target
}

subset_to_matrix <- function(data, var, class, equal = TRUE) {
  if (equal) {
    return(as.matrix(data[data[[var]] == class, names(data) != var]))
  } else {
    return(as.matrix(data[data[[var]] != class, names(data) != var]))
  }
}
