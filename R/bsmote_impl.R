#' borderline-SMOTE Algorithm
#'
#' BSMOTE generates new examples of the minority class using nearest
#'  neighbors of these cases in the border region between classes.
#'
#' @inheritParams step_smote
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#' @param all_neighbors Type of two borderline-SMOTE method. Defaults to FALSE.
#'  See details.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-bsmote
#'
#' @template details-smote
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Hui Han, Wen-Yuan Wang, and Bing-Huan Mao. Borderline-smote:
#' a new over-sampling method in imbalanced data sets learning. In
#' International Conference on Intelligent Computing, pages 878–887. Springer,
#' 2005.
#'
#' @seealso [step_bsmote()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- bsmote(circle_numeric, var = "class")
#'
#' res <- bsmote(circle_numeric, var = "class", k = 10)
#'
#' res <- bsmote(circle_numeric, var = "class", over_ratio = 0.8)
#'
#' res <- bsmote(circle_numeric, var = "class", all_neighbors = TRUE)
#'
#' res <- bsmote(circle_numeric, var = "class", distance = "manhattan")
bsmote <- function(
  df,
  var,
  k = 5,
  over_ratio = 1,
  all_neighbors = FALSE,
  distance = "euclidean"
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)
  check_bool(all_neighbors)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  bsmote_impl(df, var, k, over_ratio, all_neighbors, distance)
}

bsmote_impl <- function(
  df,
  var,
  k = 5,
  over_ratio = 1,
  all_neighbors = FALSE,
  distance = "euclidean",
  call = caller_env()
) {
  df[[var]] <- as.factor(df[[var]])
  counts <- table(drop_unused_levels(df[[var]]))
  majority_count <- max(counts)
  ratio_target <- round(majority_count * over_ratio)
  which_upsample <- which(counts < ratio_target)
  samples_needed <- ratio_target - counts[which_upsample]
  min_names <- names(samples_needed)
  out_dfs <- list()
  data_mat <- as.matrix(df[names(df) != var])
  ids <- nn_indices(data_mat, k, distance)
  for (i in seq_along(min_names)) {
    min_class_in <- df[[var]] == min_names[i]

    danger_ids <- danger(
      x = rowSums(matrix((min_class_in)[ids], ncol = ncol(ids))) - 1,
      k = k
    )

    if (sum(danger_ids) <= k) {
      cli::cli_abort(
        c(
          "The minority class {.val {min_names[i]}} does not have enough danger observations to perform BSMOTE.",
          i = "{sum(danger_ids)} danger observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
        ),
        call = call
      )
    }

    if (all_neighbors) {
      # borderline-SMOTE2 seeds from minority danger points but interpolates
      # toward all neighbors, taking a reduced step toward majority neighbors.
      tmp_df <- as.data.frame(
        smote_data(
          data = data_mat,
          k = k,
          n_samples = samples_needed[i],
          smote_ids = which(danger_ids & min_class_in),
          distance = distance,
          majority_neighbors = !min_class_in
        )
      )
    } else {
      tmp_df <- as.data.frame(
        smote_data(
          data = data_mat[min_class_in, , drop = FALSE],
          k = k,
          n_samples = samples_needed[i],
          smote_ids = which(danger_ids[min_class_in]),
          distance = distance
        )
      )
    }

    colnames(tmp_df) <- colnames(data_mat)
    tmp_df[[var]] <- min_names[i]
    out_dfs[[i]] <- tmp_df
  }

  final <- rbind(df, do.call(rbind, out_dfs))
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

danger <- function(x, k) {
  # `x` is the number of minority-class neighbors (self excluded), so there are
  # `k - x` majority-class neighbors. Han (2005) puts a point in danger when
  #
  #   k / 2 <= (majority neighbors) < k
  #
  # which, substituting `k - x`, is equivalent to `0 < x <= k / 2`. A point with
  # `x == 0` (all majority neighbors) is noise and is excluded.
  x > 0 & x <= k / 2
}
