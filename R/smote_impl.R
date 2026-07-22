#' SMOTE Algorithm
#'
#' SMOTE generates new examples of the minority class using nearest neighbors
#' of these cases.
#'
#' @inheritParams step_smote
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-smote
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer,
#'  W. P. (2002). Smote: Synthetic minority over-sampling technique.
#'  Journal of Artificial Intelligence Research, 16:321-357.
#'
#' @seealso [step_smote()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- smote(circle_numeric, var = "class")
#'
#' res <- smote(circle_numeric, var = "class", k = 10)
#'
#' res <- smote(circle_numeric, var = "class", over_ratio = 0.8)
#'
#' res <- smote(circle_numeric, var = "class", distance = "manhattan")
smote <- function(df, var, k = 5, over_ratio = 1, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  smote_impl(df, var, k, over_ratio, distance)
}

smote_impl <- function(
  df,
  var,
  k,
  over_ratio,
  distance = "euclidean",
  call = caller_env()
) {
  df[[var]] <- as.factor(df[[var]])
  data <- split(df, df[[var]])
  counts <- table(drop_unused_levels(df[[var]]))
  majority_count <- max(counts)
  ratio_target <- round(majority_count * over_ratio)
  which_upsample <- which(counts < ratio_target)
  samples_needed <- ratio_target - counts[which_upsample]
  min_names <- names(samples_needed)

  out_dfs <- list()

  for (i in seq_along(samples_needed)) {
    minority_df <- data[[min_names[i]]]
    minority <- as.matrix(minority_df[names(minority_df) != var])

    if (nrow(minority) <= k) {
      cli::cli_abort(
        c(
          "The minority class {.val {min_names[i]}} does not have enough observations to perform SMOTE.",
          i = "{nrow(minority)} observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
        ),
        call = call
      )
    }

    synthetic <- smote_data(
      minority,
      k = k,
      n_samples = samples_needed[i],
      distance = distance
    )
    out_df <- as.data.frame(synthetic)
    names(out_df) <- setdiff(names(df), var)
    out_df_nrow <- min(nrow(out_df), 1)
    out_df[var] <- data[[names(samples_needed)[i]]][[var]][out_df_nrow]
    out_df <- out_df[names(df)]
    out_dfs[[i]] <- out_df
  }

  final <- rbind(df, do.call(rbind, out_dfs))
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}


smote_data <- function(
  data,
  k,
  n_samples,
  smote_ids = seq_len(nrow(data)),
  distance = "euclidean",
  step_size = 1,
  majority_neighbors = NULL
) {
  ids <- nn_indices(data, k, distance)
  indexes <- rep(sample(smote_ids), length.out = n_samples)
  index_len <- tabulate(indexes, NROW(data))
  out <- matrix(0, nrow = n_samples, ncol = ncol(data))
  sampleids <- sample.int(k, n_samples, TRUE)
  runif_ids <- stats::runif(n_samples)

  iii <- 0
  for (row_num in smote_ids) {
    index_selection <- iii + seq_len(index_len[row_num])
    # removes itself as nearest neighbour
    id_knn <- ids[row_num, ids[row_num, ] != row_num]
    selected <- id_knn[sampleids[index_selection]]
    dif <- data[selected, ] - data[rep(row_num, index_len[row_num]), ]
    step <- rep(step_size, length(index_selection))
    if (!is.null(majority_neighbors)) {
      # borderline-SMOTE2 halves the step toward majority-class neighbors so
      # synthetic points stay closer to the minority seed.
      step[majority_neighbors[selected]] <- step[majority_neighbors[selected]] *
        0.5
    }
    gap <- dif * runif_ids[index_selection] * step
    out[index_selection, ] <- data[rep(row_num, index_len[row_num]), ] + gap
    iii <- iii + index_len[row_num]
  }

  out
}
