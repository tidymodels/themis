#' Remove Tomek's links
#'
#' Removes the majority class member of each pair of observations that form a
#' Tomek link.
#'
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @inheritParams step_smote
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-tomek
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Tomek. Two modifications of cnn. IEEE Trans. Syst. Man Cybern.,
#'  6:769-772, 1976.
#'
#' @seealso [step_tomek()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- tomek(circle_numeric, var = "class")
#'
#' res <- tomek(circle_numeric, var = "class", distance = "manhattan")
tomek <- function(df, var, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  df[-tomek_impl(df, var, distance), ]
}

tomek_impl <- function(df, var, distance = "euclidean") {
  res <- nn_indices(as.matrix(df[names(df) != var]), k = 1, distance)
  # Drop each observation from its own neighbor list (by row index, so exact
  # duplicates are handled correctly) and keep the single nearest neighbor
  res <- drop_self_neighbor(res)[, 1]

  remove <- logical(nrow(df))
  outcome <- df[[var]]
  class_sizes <- table(outcome)

  for (class in unique(outcome)) {
    target <- which(outcome == class)
    neighbor <- res[target]
    neighbor_neighbor <- res[neighbor]

    tomek <- target == neighbor_neighbor & outcome[target] != outcome[neighbor]

    target_link <- target[tomek]
    neighbor_link <- neighbor[tomek]

    # Remove only the majority-class member of each Tomek link
    target_is_majority <-
      class_sizes[as.character(outcome[target_link])] >=
        class_sizes[as.character(outcome[neighbor_link])]

    remove[ifelse(target_is_majority, target_link, neighbor_link)] <- TRUE
  }

  which(remove)
}
