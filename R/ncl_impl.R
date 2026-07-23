#' Neighborhood Cleaning Rule
#'
#' Under-samples the majority classes by cleaning noisy observations and
#' observations that pollute the neighborhood of minority class observations.
#'
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @inheritParams step_ncl
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-ncl
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Laurikkala, J. (2001). Improving identification of difficult
#' small classes by balancing class distribution. In Conference on Artificial
#' Intelligence in Medicine in Europe (pp. 63-66). Springer.
#'
#' @seealso [step_ncl()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- ncl(circle_numeric, var = "class")
#'
#' res <- ncl(circle_numeric, var = "class", neighbors = 5)
#'
#' res <- ncl(circle_numeric, var = "class", distance = "manhattan")
ncl <- function(
  df,
  var,
  neighbors = 3,
  distance = "euclidean",
  threshold_clean = 0.5
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(neighbors, min = 1)
  check_distance_arg(distance)
  check_number_decimal(threshold_clean, min = 0)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  remove <- ncl_impl(
    df,
    var,
    neighbors,
    distance,
    threshold_clean
  )
  if (length(remove) > 0) {
    df <- df[-remove, ]
  }
  df
}

ncl_impl <- function(
  df,
  var,
  neighbors = 3,
  distance = "euclidean",
  threshold_clean = 0.5,
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

  outcome <- as.character(df[[var]])

  counts <- table(df[[var]])
  minority <- names(counts)[which.min(counts)]
  minority_count <- min(counts)

  # Majority classes large enough to be cleaned around minority observations.
  classes_to_clean <- setdiff(
    names(counts)[counts >= minority_count * threshold_clean],
    minority
  )

  idx <- nn_indices(as.matrix(df[names(df) != var]), k = neighbors, distance)
  # Drop each observation from its own neighbor list (by row index, so exact
  # duplicates are handled correctly)
  idx <- drop_self_neighbor(idx)

  neighbor_classes <- matrix(
    outcome[idx],
    nrow = nrow(idx),
    ncol = ncol(idx)
  )
  neighbor_mode <- apply(neighbor_classes, 1, Mode)

  misclassified <- outcome != as.character(neighbor_mode)

  # A1: ENN removes misclassified majority observations.
  a1 <- which(misclassified & outcome != minority)

  # A2: for minority observations misclassified by their neighbors, remove the
  # neighbors that belong to a majority class large enough to be cleaned.
  a2 <- integer(0)
  minority_bad <- which(misclassified & outcome == minority)
  if (length(minority_bad) > 0 && length(classes_to_clean) > 0) {
    candidates <- unique(as.vector(idx[minority_bad, , drop = FALSE]))
    a2 <- candidates[outcome[candidates] %in% classes_to_clean]
  }

  sort(unique(c(a1, a2)))
}
