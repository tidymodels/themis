#' Edited Nearest Neighbors
#'
#' Removes observations whose class differs from the majority of their nearest
#' neighbors.
#'
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @inheritParams step_enn
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Wilson, D. L. (1972). Asymptotic properties of nearest neighbor
#' rules using edited data. IEEE Transactions on Systems, Man, and Cybernetics,
#' (3), 408-421.
#'
#' @seealso [step_enn()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- enn(circle_numeric, var = "class")
#'
#' res <- enn(circle_numeric, var = "class", neighbors = 5)
#'
#' res <- enn(circle_numeric, var = "class", distance = "manhattan")
enn <- function(df, var, neighbors = 3, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(neighbors, min = 1)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  remove <- enn_impl(df, var, neighbors, distance)
  if (length(remove) > 0) {
    df <- df[-remove, ]
  }
  df
}

enn_impl <- function(
  df,
  var,
  neighbors = 3,
  distance = "euclidean",
  call = caller_env()
) {
  outcome <- df[[var]]

  if (nrow(df) <= neighbors) {
    cli::cli_abort(
      c(
        "Not enough observations to compute {neighbors} nearest neighbors.",
        i = "{nrow(df)} observation{?s} {?was/were} found, but {neighbors + 1} {?is/are} needed."
      ),
      call = call
    )
  }

  idx <- nn_indices(as.matrix(df[names(df) != var]), k = neighbors, distance)
  # First column is the observation itself; drop it
  idx <- idx[, -1, drop = FALSE]

  neighbor_classes <- matrix(
    outcome[idx],
    nrow = nrow(idx),
    ncol = ncol(idx)
  )

  neighbor_mode <- apply(neighbor_classes, 1, Mode)

  which(as.character(outcome) != as.character(neighbor_mode))
}
