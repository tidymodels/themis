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
#' Setting `all_k = TRUE` applies ENN with increasing numbers of neighbors, from
#' `1` up to `neighbors`, cleaning the data at each step. This corresponds to
#' All k-Nearest Neighbors (AllKNN) and takes precedence over `times`.
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
#'
#' # All k-Nearest Neighbors (AllKNN)
#' res <- enn(circle_numeric, var = "class", all_k = TRUE)
enn <- function(
  df,
  var,
  neighbors = 3,
  distance = "euclidean",
  times = 1,
  all_k = FALSE
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(neighbors, min = 1)
  check_distance_arg(distance)
  check_number_whole(times, min = 1, allow_infinite = TRUE)
  check_bool(all_k)
  warn_times_all_k(times, all_k)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  remove <- enn_impl(df, var, neighbors, distance, times = times, all_k = all_k)
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
  all_k = FALSE,
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

  # Number of neighbors to use on each pass. AllKNN increases k from 1 up to
  # `neighbors`; RENN repeats with a fixed k up to `times` passes.
  if (all_k) {
    ks <- seq_len(neighbors)
  } else {
    ks <- rep(neighbors, min(times, nrow(df)))
  }

  # Row indices (into the original `df`) that are still active
  active <- seq_len(nrow(df))

  for (k in ks) {
    # Not enough observations left to keep cleaning
    if (length(active) <= k) {
      break
    }

    remove <- enn_single(df[active, , drop = FALSE], var, k, distance)

    if (length(remove) == 0) {
      # RENN stops early at convergence; AllKNN continues with a larger k
      if (!all_k) {
        break
      }
      next
    }

    active <- active[-remove]
  }

  setdiff(seq_len(nrow(df)), active)
}

# Warn when both `times` and `all_k` are set, since `all_k` takes precedence.
warn_times_all_k <- function(times, all_k, call = caller_env()) {
  if (all_k && times != 1) {
    cli::cli_warn(
      "{.arg times} is ignored when {.arg all_k} is {.code TRUE}.",
      call = call
    )
  }
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
