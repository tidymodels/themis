#' Remove hard to classify points
#'
#' Under-samples the majority classes by removing the points that are hardest
#' to classify.
#'
#' @inheritParams step_instance_hardness
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbors used to estimate the
#'  instance hardness of each observation.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-instance_hardness
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Smith, M. R., Martinez, T., & Giraud-Carrier, C. (2014). An
#' instance level analysis of data complexity. Machine learning, 95(2),
#' 225-256.
#'
#' @seealso [step_instance_hardness()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- instance_hardness(circle_numeric, var = "class")
#'
#' res <- instance_hardness(circle_numeric, var = "class", k = 10)
#'
#' res <- instance_hardness(circle_numeric, var = "class", under_ratio = 1.5)
#'
#' res <- instance_hardness(circle_numeric, var = "class", distance = "manhattan")
instance_hardness <- function(
  df,
  var,
  k = 5,
  under_ratio = 1,
  distance = "euclidean"
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(under_ratio)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  instance_hardness_impl(
    df,
    var,
    ignore_vars = character(),
    k,
    under_ratio,
    distance = distance
  )
}

instance_hardness_impl <- function(
  df,
  var,
  ignore_vars,
  k = 5,
  under_ratio = 1,
  distance = "euclidean",
  call = caller_env()
) {
  classes <- downsample_count(df, var, under_ratio)

  if (length(classes) == 0) {
    return(df)
  }

  df_only <- df[, !names(df) %in% ignore_vars]
  full <- as.matrix(df_only[, names(df_only) != var])
  labels <- df[[var]]

  if (nrow(full) <= k) {
    cli::cli_abort(
      c(
        "Not enough observations to compute {k} nearest neighbors.",
        i = "{nrow(full)} observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
      ),
      call = call
    )
  }

  idx <- nn_indices(full, k, distance)

  deleted_rows <- integer()
  for (i in seq_along(classes)) {
    which_rows <- which(labels == names(classes)[i])

    neigh_idx <- idx[which_rows, , drop = FALSE]
    neigh_labels <- matrix(labels[neigh_idx], nrow = nrow(neigh_idx))
    disagree <- neigh_labels != labels[which_rows]
    disagree[neigh_idx == which_rows] <- NA
    hardness <- rowMeans(disagree, na.rm = TRUE)

    n_keep <- length(which_rows) - classes[i]
    selected_ind <- rank(hardness, ties.method = "first") <= n_keep
    deleted_rows <- c(deleted_rows, which_rows[!selected_ind])
  }

  if (length(deleted_rows) > 0) {
    df <- df[-deleted_rows, ]
  }
  df
}
