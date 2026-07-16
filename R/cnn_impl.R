#' Condensed Nearest Neighbors
#'
#' Under-samples the majority classes by keeping only a consistent subset of
#' observations that correctly classifies the data using a 1-nearest-neighbor
#' rule.
#'
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @inheritParams step_cnn
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-cnn
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Hart, P. (1968). The condensed nearest neighbor rule. IEEE
#' Transactions on Information Theory, 14(3), 515-516.
#'
#' @seealso [step_cnn()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- cnn(circle_numeric, var = "class")
#'
#' res <- cnn(circle_numeric, var = "class", distance = "manhattan")
cnn <- function(df, var, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  remove <- cnn_impl(df, var, distance)
  if (length(remove) > 0) {
    df <- df[-remove, ]
  }
  df
}

cnn_impl <- function(df, var, distance = "euclidean", call = caller_env()) {
  outcome <- as.character(df[[var]])
  predictors <- as.matrix(df[names(df) != var])

  counts <- table(outcome)
  minority <- names(counts)[which.min(counts)]

  minority_idx <- which(outcome == minority)
  majority_idx <- which(outcome != minority)

  if (length(majority_idx) == 0) {
    return(integer(0))
  }

  # Seed the store with all minority observations and one random majority one.
  in_store <- logical(nrow(df))
  in_store[minority_idx] <- TRUE
  in_store[majority_idx[sample.int(length(majority_idx), 1)]] <- TRUE

  repeat {
    added <- FALSE
    # Scan order is randomized; CNN is order-dependent.
    candidates <- sample(majority_idx[!in_store[majority_idx]])

    for (i in candidates) {
      store_idx <- which(in_store)
      nn <- nn_indices_cross(
        predictors[i, , drop = FALSE],
        predictors[store_idx, , drop = FALSE],
        k = 1,
        distance = distance
      )
      if (outcome[store_idx[nn[1, 1]]] != outcome[i]) {
        in_store[i] <- TRUE
        added <- TRUE
      }
    }

    if (!added) {
      break
    }
  }

  # Majority observations left outside the store are removed.
  which(!in_store)
}
