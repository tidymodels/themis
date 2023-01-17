#' Remove Tomek's links
#'
#' Removed observations that are part of tomek links.
#'
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
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
tomek <- function(df, var) {
  if (length(var) != 1) {
    rlang::abort("Please select a single factor variable for `var`.")
  }

  var <- rlang::arg_match(var, colnames(df))

  if (!(is.factor(df[[var]]) | is.character(df[[var]]))) {
    rlang::abort(glue("`{var}` should be a factor or character variable."))
  }

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  df[-tomek_impl(df, var), ]
}

tomek_impl <- function(df, var) {
  res <- RANN::nn2(df[names(df) != var], k = 2)$nn.idx
  # Make sure itself isn't counted as nearest neighbor for overlaps
  res <- dplyr::if_else(seq_len(nrow(res)) == res[, 2], res[, 1], res[, 2])

  remove <- logical(nrow(df))
  outcome <- df[[var]]

  for (class in unique(outcome)) {
    target <- which(outcome == class)
    neighbor <- res[target]
    neighbor_neighbor <- res[neighbor]

    tomek <- target == neighbor_neighbor & outcome[target] != outcome[neighbor]

    tomek_links <- c(target[tomek], neighbor[tomek])
    remove[tomek_links] <- TRUE
  }

  which(remove)
}
