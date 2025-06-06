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
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer,
#'  W. P. (2002). Smote: Synthetic minority over-sampling technique.
#'  Journal of Artificial Intelligence Research, 16:321-357.
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
adasyn <- function(df, var, k = 5, over_ratio = 1) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  adasyn_impl(df, var, k, over_ratio)
}

adasyn_impl <- function(df, var, k = 5, over_ratio = 1, call = caller_env()) {
  majority_count <- max(table(df[[var]]))
  ratio_target <- majority_count * over_ratio
  which_upsample <- which(table(df[[var]]) < ratio_target)
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  min_names <- names(samples_needed)
  out_dfs <- list()

  data_mat <- as.matrix(df[names(df) != var])
  ids_full <- RANN::nn2(data_mat, k = k + 1, searchtype = "priority")$nn.idx

  for (i in seq_along(min_names)) {
    min_class_in <- df[[var]] != min_names[i]

    r_value <- pmax(
      0,
      rowSums(matrix((min_class_in)[ids_full], ncol = ncol(ids_full))) - 1
    )
    r_value <- r_value[!min_class_in]
    danger_ids <- sample(
      seq_along(r_value),
      samples_needed[i],
      TRUE,
      prob = r_value
    )

    minority <- data_mat[!min_class_in, , drop = FALSE]

    if (nrow(minority) <= k) {
      cli::cli_abort(
        "Not enough observations of {.val {min_names[i]}} to perform ADASYN.",
        call = call
      )
    }

    tmp_df <- as.data.frame(
      adasyn_sampler(
        minority,
        k,
        samples_needed[i],
        danger_ids
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

adasyn_sampler <- function(data, k, n_samples, smote_ids) {
  ids <- RANN::nn2(data, k = k + 1, searchtype = "priority")$nn.idx
  index_len <- tabulate(smote_ids, NROW(data))
  out <- matrix(0, nrow = n_samples, ncol = ncol(data))
  sampleids <- sample.int(k, n_samples, TRUE)
  runif_ids <- stats::runif(n_samples)

  iii <- 0
  for (row_num in which(index_len != 0)) {
    index_selection <- iii + seq_len(index_len[row_num])
    # removes itself as nearest neighbour
    id_knn <- ids[row_num, ids[row_num, ] != row_num]
    dif <- data[id_knn[sampleids[index_selection]], ] -
      data[rep(row_num, index_len[row_num]), ]
    gap <- dif * runif_ids[index_selection]
    out[index_selection, ] <- data[rep(row_num, index_len[row_num]), ] + gap
    iii <- iii + index_len[row_num]
  }

  out
}
