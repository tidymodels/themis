#' Adaptive Synthetic Algorithm
#'
#' Generates synthetic positive instances using ADASYN algorithm.
#'
#' @inheritParams step_adasyn
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-adasyn
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references He, H., Bai, Y., Garcia, E. and Li, S. 2008. ADASYN: Adaptive
#'  synthetic sampling approach for imbalanced learning. Proceedings of
#'  IJCNN 2008. (IEEE World Congress on Computational Intelligence). IEEE
#'  International Joint Conference. pp.1322-1328.
#'
#' @seealso [step_adasyn()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- adasyn(circle_numeric, var = "class")
#'
#' res <- adasyn(circle_numeric, var = "class", k = 10)
#'
#' res <- adasyn(circle_numeric, var = "class", over_ratio = 0.8)
#'
#' res <- adasyn(circle_numeric, var = "class", distance = "manhattan")
adasyn <- function(df, var, k = 5, over_ratio = 1, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  adasyn_impl(df, var, k, over_ratio, distance)
}

adasyn_impl <- function(
  df,
  var,
  k = 5,
  over_ratio = 1,
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

  for (i in seq_along(min_names)) {
    min_class_in <- df[[var]] != min_names[i]

    minority <- data_mat[!min_class_in, , drop = FALSE]

    if (nrow(minority) <= k) {
      cli::cli_abort(
        c(
          "The minority class {.val {min_names[i]}} does not have enough observations to perform ADASYN.",
          i = "{nrow(minority)} observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
        ),
        call = call
      )
    }

    # r_value counts the majority-class neighbors among each minority point's
    # k + 1 nearest neighbors in the full data. Query only the minority rows
    # against the full data instead of computing neighbors for every row and
    # discarding the majority ones; the neighbor sets (and thus r_value) are
    # identical.
    ids <- nn_indices_cross(minority, data_mat, k + 1, distance)
    r_value <- rowSums(matrix((min_class_in)[ids], ncol = ncol(ids)))
    # When the minority class is well separated from the majority classes all
    # weights are 0, which `sample()` cannot use. Fall back to uniform sampling.
    if (all(r_value == 0)) {
      r_value <- NULL
    }
    danger_ids <- sample(
      seq_along(minority[, 1]),
      samples_needed[i],
      TRUE,
      prob = r_value
    )

    tmp_df <- as.data.frame(
      adasyn_sampler(
        minority,
        k,
        samples_needed[i],
        danger_ids,
        distance
      )
    )

    colnames(tmp_df) <- colnames(data_mat)
    tmp_df[[var]] <- min_names[i]
    out_dfs[[i]] <- tmp_df
  }
  final <- rbind(df, do.call(rbind, out_dfs))
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

adasyn_sampler <- function(
  data,
  k,
  n_samples,
  smote_ids,
  distance = "euclidean"
) {
  ids <- drop_self_neighbor(nn_indices(data, k, distance))
  index_len <- tabulate(smote_ids, NROW(data))
  out <- matrix(0, nrow = n_samples, ncol = ncol(data))
  sampleids <- sample.int(k, n_samples, TRUE)
  runif_ids <- stats::runif(n_samples)

  iii <- 0
  for (row_num in which(index_len != 0)) {
    index_selection <- iii + seq_len(index_len[row_num])
    # self already removed by drop_self_neighbor()
    id_knn <- ids[row_num, ]
    dif <- data[id_knn[sampleids[index_selection]], ] -
      data[rep(row_num, index_len[row_num]), ]
    gap <- dif * runif_ids[index_selection]
    out[index_selection, ] <- data[rep(row_num, index_len[row_num]), ] + gap
    iii <- iii + index_len[row_num]
  }

  out
}
