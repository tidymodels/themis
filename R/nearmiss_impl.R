#' Remove Points Near Other Classes
#'
#' Generates synthetic positive instances using nearmiss algorithm.
#'
#' @inheritParams step_nearmiss
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Inderjeet Mani and I Zhang. knn approach to unbalanced data
#' distributions: a case study involving information extraction. In Proceedings
#' of workshop on learning from imbalanced datasets, 2003.
#'
#' @seealso [step_nearmiss()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- nearmiss(circle_numeric, var = "class")
#'
#' res <- nearmiss(circle_numeric, var = "class", k = 10)
#'
#' res <- nearmiss(circle_numeric, var = "class", under_ratio = 1.5)
nearmiss <- function(df, var, k = 5, under_ratio = 1) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(under_ratio)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  nearmiss_impl(df, var, ignore_vars = character(), k, under_ratio)
}

nearmiss_impl <- function(df, var, ignore_vars, k = 5, under_ratio = 1) {
  classes <- downsample_count(df, var, under_ratio)

  out_dfs <- list()

  deleted_rows <- integer()
  for (i in seq_along(classes)) {
    df_only <- df[, !names(df) %in% ignore_vars]
    class <- subset_to_matrix(df_only, var, names(classes)[i])
    not_class <- subset_to_matrix(df_only, var, names(classes)[i], FALSE)

    if (nrow(not_class) <= k) {
      cli::cli_abort("Not enough danger observations of {.val {names(classes)[i]}} to perform NEARMISS.")
    }

    dists <- RANN::nn2(
      not_class[, !(colnames(not_class) %in% ignore_vars)],
      class[, !(colnames(class) %in% ignore_vars)],
      k = k
    )$nn.dists

    selected_ind <- order(rowMeans(dists)) <= (nrow(class) - classes[i])
    deleted_rows <- c(deleted_rows, which(df[[var]] %in% names(classes)[i])[!selected_ind])
  }

  if (length(deleted_rows) > 0) {
    df <- df[-deleted_rows, ]
  }
  df
}

downsample_count <- function(data, var, ratio) {
  min_count <- min(table(data[[var]]))
  ratio_target <- min_count * ratio
  which_class <- which(table(data[[var]]) > ratio_target)
  table(data[[var]])[which_class] - ratio_target
}

subset_to_matrix <- function(data, var, class, equal = TRUE) {
  if (equal) {
    return(as.matrix(data[data[[var]] == class, names(data) != var]))
  } else {
    return(as.matrix(data[data[[var]] != class, names(data) != var]))
  }
}
