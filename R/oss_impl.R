#' One-Sided Selection
#'
#' Under-samples the majority classes by combining Condensed Nearest Neighbors
#' and Tomek's links, first reducing redundant majority class observations and
#' then removing majority class observations that form Tomek links with minority
#' class observations.
#'
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @inheritParams step_oss
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-oss
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Kubat, M., & Matwin, S. (1997). Addressing the curse of
#' imbalanced training sets: one-sided selection. In ICML (Vol. 97, pp.
#' 179-186).
#'
#' @seealso [step_oss()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- oss(circle_numeric, var = "class")
#'
#' res <- oss(circle_numeric, var = "class", distance = "manhattan")
oss <- function(df, var, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  remove <- oss_impl(df, var, distance)
  if (length(remove) > 0) {
    df <- df[-remove, ]
  }
  df
}

oss_impl <- function(df, var, distance = "euclidean", call = caller_env()) {
  outcome <- as.character(df[[var]])

  counts <- table(outcome)
  minority <- names(counts)[which.min(counts)]

  # Step 1: CNN condensing removes redundant majority class observations.
  cnn_removed <- cnn_impl(df, var, distance = distance, call = call)

  keep <- setdiff(seq_len(nrow(df)), cnn_removed)
  kept_df <- df[keep, , drop = FALSE]

  # Step 2: Tomek links, only removing majority class observations.
  tomek_local <- tomek_impl(kept_df, var, distance = distance)
  tomek_removed <- keep[tomek_local]
  tomek_removed <- tomek_removed[outcome[tomek_removed] != minority]

  sort(unique(c(cnn_removed, tomek_removed)))
}
