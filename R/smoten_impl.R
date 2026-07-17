#' SMOTEN Algorithm
#'
#' SMOTEN generates new examples of the minority class using nearest neighbors
#' of these cases, for data sets where all predictors are categorical.
#'
#' @inheritParams step_smoten
#' @param df data.frame or tibble. Must have 1 factor variable used as the
#'  outcome and remaining categorical (factor or character) variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-smoten
#'
#' @details
#' All columns other than `var` must be categorical (factor or character) with
#' no missing data.
#'
#' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer,
#'  W. P. (2002). Smote: Synthetic minority over-sampling technique.
#'  Journal of Artificial Intelligence Research, 16:321-357.
#'
#' @seealso [step_smoten()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' df <- data.frame(
#'   x = factor(sample(letters[1:3], 100, replace = TRUE)),
#'   y = factor(sample(letters[1:2], 100, replace = TRUE)),
#'   class = factor(c(rep("minority", 20), rep("majority", 80)))
#' )
#'
#' res <- smoten(df, var = "class")
#'
#' res <- smoten(df, var = "class", k = 10)
#'
#' res <- smoten(df, var = "class", over_ratio = 0.8)
smoten <- function(df, var, k = 5, over_ratio = 1) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)

  check_na(select(df, -all_of(var)))
  check_all_categorical(select(df, -all_of(var)))

  smoten_impl(df, var, k, over_ratio)
}

# Splits data and appends new minority instances
smoten_impl <- function(df, var, k, over_ratio, call = caller_env()) {
  predictors <- setdiff(names(df), var)
  # per-feature Value Difference Metric between category levels
  deltas <- vdm_deltas(df, var, predictors)

  # split data into list names by classes
  data <- split(df, df[[var]])
  # Number of majority instances
  majority_count <- max(table(df[[var]]))
  # How many minority samples do we want in total?
  ratio_target <- majority_count * over_ratio
  # Which classes need upsampling
  which_upsample <- which(table(df[[var]]) < ratio_target)
  # For each minority class, determine how many more samples are needed
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  min_names <- names(samples_needed)

  out_dfs <- list()

  for (i in seq_along(samples_needed)) {
    minority <- data[[min_names[i]]]

    if (nrow(minority) <= k) {
      cli::cli_abort(
        c(
          "The minority class {.val {min_names[i]}} does not have enough observations to perform SMOTEN.",
          i = "{nrow(minority)} observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
        ),
        call = call
      )
    }

    out_dfs[[i]] <- smoten_data(
      minority,
      k = k,
      n_samples = samples_needed[i],
      deltas = deltas,
      predictors = predictors
    )
  }

  final <- rbind(df, do.call(rbind, out_dfs))
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

# Value Difference Metric: for each predictor, distance between each pair of
# category levels based on their conditional outcome-class distributions.
vdm_deltas <- function(df, var, predictors) {
  y <- factor(df[[var]])
  deltas <- list()
  for (p in predictors) {
    x <- factor(df[[p]])
    tab <- table(x, y)
    probs <- tab / rowSums(tab)
    probs[is.nan(probs)] <- 0
    d <- as.matrix(stats::dist(probs, method = "manhattan"))
    deltas[[p]] <- d
  }
  deltas
}

# Pairwise VDM distances between all rows of a single-class data frame
vdm_dist_matrix <- function(data, deltas, predictors) {
  n <- nrow(data)
  out <- matrix(0, nrow = n, ncol = n)
  for (p in predictors) {
    lev <- as.character(data[[p]])
    out <- out + deltas[[p]][lev, lev]
  }
  out
}

# Uses nearest-neighbors and majority voting to generate new instances
smoten_data <- function(
  data,
  k,
  n_samples,
  deltas,
  predictors,
  smoten_ids = seq_len(nrow(data))
) {
  dist_mat <- vdm_dist_matrix(data, deltas, predictors)
  # ordering of neighbors for each row (closest first)
  order_mat <- t(apply(dist_mat, 1, order))

  # shuffle minority indices, repeated to reach the desired number of samples
  indexes <- rep(sample(smoten_ids), length.out = n_samples)

  out <- data[indexes, , drop = FALSE]

  for (i in seq_along(indexes)) {
    row_num <- indexes[i]
    id_knn <- order_mat[row_num, order_mat[row_num, ] != row_num][seq_len(k)]
    neighbors <- data[id_knn, predictors, drop = FALSE]
    for (p in predictors) {
      out[i, p] <- Mode(neighbors[[p]])
    }
  }

  rownames(out) <- NULL
  out
}
