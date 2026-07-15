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
#' Setting `times` greater than 1 applies ENN repeatedly. Each pass removes
#' observations from the data before the next pass runs, stopping early once a
#' pass removes nothing (convergence). This corresponds to Repeated Edited
#' Nearest Neighbors (RENN). Use `times = Inf` to repeat until convergence.
#'
#' @references Wilson, D. L. (1972). Asymptotic properties of nearest neighbor
#' rules using edited data. IEEE Transactions on Systems, Man, and Cybernetics,
#' (3), 408-421.
#'
#' Tomek, I. (1976). An experiment with the edited nearest-neighbor rule. IEEE
#' Transactions on Systems, Man, and Cybernetics, (6), 448-452.
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
#'
#' # Repeated Edited Nearest Neighbors (RENN)
#' res <- enn(circle_numeric, var = "class", times = Inf)
enn <- function(df, var, neighbors = 3, distance = "euclidean", times = 1) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(neighbors, min = 1)
  check_distance_arg(distance)
  check_number_whole(times, min = 1, allow_infinite = TRUE)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  remove <- enn_impl(df, var, neighbors, distance, times = times)
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
  times = 1,
  call = caller_env()
) {
  if (nrow(df) <= neighbors) {
    cli::cli_abort(
      c(
        "Not enough observations to compute {neighbors} nearest neighbors.",
        i = "{nrow(df)} observation{?s} {?was/were} found, but {neighbors + 1} {?is/are} needed."
      ),
      call = call
    )
  }

  # Row indices (into the original `df`) that are still active
  active <- seq_len(nrow(df))
  iter <- 0

  while (iter < times) {
    iter <- iter + 1

    # Not enough observations left to keep cleaning
    if (length(active) <= neighbors) {
      break
    }

    remove <- enn_single(df[active, , drop = FALSE], var, neighbors, distance)

    if (length(remove) == 0) {
      break
    }

    active <- active[-remove]
  }

  setdiff(seq_len(nrow(df)), active)
}

# A single pass of Edited Nearest Neighbors. Returns the row indices of `df`
# that should be removed.
enn_single <- function(df, var, neighbors, distance) {
  outcome <- df[[var]]

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
