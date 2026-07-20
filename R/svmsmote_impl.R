#' SVM-SMOTE Algorithm
#'
#' SVM-SMOTE generates new examples of the minority class near the decision
#'  boundary, using the support vectors of a fitted SVM to decide where to
#'  place synthetic examples.
#'
#' @inheritParams step_svmsmote
#' @param df data.frame or tibble. Must have 1 factor variable and remaining
#'  numeric variables.
#' @param var Character, name of variable containing factor variable.
#' @param k An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @template details-svmsmote
#'
#' @template details-smote
#'
#' @details
#' All columns used in this function must be numeric with no missing data.
#'
#' @references Nguyen, H. M., Cooper, E. W., and Kamei, K. (2011). Borderline
#'  over-sampling for imbalanced data classification. International Journal of
#'  Knowledge Engineering and Soft Data Paradigms, 3(1), 4-21.
#'
#' @seealso [step_svmsmote()] for step function of this method
#' @family Direct Implementations
#'
#' @examplesIf rlang::is_installed("kernlab")
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- svmsmote(circle_numeric, var = "class")
#'
#' res <- svmsmote(circle_numeric, var = "class", k = 10)
#'
#' res <- svmsmote(circle_numeric, var = "class", over_ratio = 0.8)
#'
#' res <- svmsmote(circle_numeric, var = "class", distance = "manhattan")
svmsmote <- function(df, var, k = 5, over_ratio = 1, distance = "euclidean") {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)
  check_distance_arg(distance)

  predictors <- setdiff(colnames(df), var)

  check_numeric(df[, predictors])
  check_na(select(df, -all_of(var)))

  svmsmote_impl(df, var, k, over_ratio, distance)
}

svmsmote_impl <- function(
  df,
  var,
  k = 5,
  over_ratio = 1,
  distance = "euclidean",
  call = caller_env()
) {
  # `m` neighbors are used to classify support vectors as noise, danger, or
  # safety; `out_step` controls how far extrapolated examples are placed.
  m <- 2 * k
  out_step <- 0.5

  majority_count <- max(table(df[[var]]))
  ratio_target <- majority_count * over_ratio
  which_upsample <- which(table(df[[var]]) < ratio_target)
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  min_names <- names(samples_needed)
  out_dfs <- list()

  data_mat <- as.matrix(df[names(df) != var])
  y <- df[[var]]

  sv_ids <- svm_support_vectors(data_mat, y, call = call)

  for (i in seq_along(min_names)) {
    min_class_in <- y == min_names[i]
    minority_mat <- data_mat[min_class_in, , drop = FALSE]

    if (nrow(minority_mat) <= k) {
      cli::cli_abort(
        c(
          "The minority class {.val {min_names[i]}} does not have enough observations to perform SVMSMOTE.",
          i = "{nrow(minority_mat)} observation{?s} {?was/were} found, but {k + 1} {?is/are} needed."
        ),
        call = call
      )
    }

    # keep only support vectors that belong to the minority class
    sv_min <- sv_ids[min_class_in[sv_ids]]

    if (length(sv_min) == 0) {
      cli::cli_abort(
        "The minority class {.val {min_names[i]}} has no support vectors to perform SVMSMOTE.",
        call = call
      )
    }

    support_vector <- data_mat[sv_min, , drop = FALSE]

    # m nearest neighbors among all classes (first column is the point itself)
    nn_m <- nn_indices_cross(support_vector, data_mat, m + 1, distance)
    nn_m <- nn_m[, -1, drop = FALSE]
    n_maj <- rowSums(matrix(!min_class_in[nn_m], nrow = nrow(support_vector)))

    noise <- n_maj == m
    danger <- (n_maj >= m / 2) & (n_maj < m)
    safety <- !danger & !noise

    if (!any(danger) && !any(safety)) {
      cli::cli_abort(
        "The minority class {.val {min_names[i]}} does not have enough non-noise support vectors to perform SVMSMOTE.",
        call = call
      )
    }

    # positions of danger/safety support vectors within the minority matrix
    minority_global <- which(min_class_in)
    danger_ids <- match(sv_min[danger], minority_global)
    safety_ids <- match(sv_min[safety], minority_global)

    n_total <- samples_needed[i]
    fraction <- stats::rbeta(1, 10, 10)
    n_danger <- round(fraction * n_total)
    n_safety <- n_total - n_danger

    if (!any(danger)) {
      n_safety <- n_total
      n_danger <- 0
    }
    if (!any(safety)) {
      n_danger <- n_total
      n_safety <- 0
    }

    gen <- list()
    if (n_danger > 0) {
      # danger points are interpolated toward minority neighbors
      gen[[1]] <- smote_data(
        minority_mat,
        k = k,
        n_samples = n_danger,
        smote_ids = danger_ids,
        distance = distance,
        step_size = 1
      )
    }
    if (n_safety > 0) {
      # safety points are extrapolated away from minority neighbors
      gen[[2]] <- smote_data(
        minority_mat,
        k = k,
        n_samples = n_safety,
        smote_ids = safety_ids,
        distance = distance,
        step_size = -out_step
      )
    }

    tmp_df <- as.data.frame(do.call(rbind, gen))
    colnames(tmp_df) <- colnames(data_mat)
    tmp_df[[var]] <- min_names[i]
    out_dfs[[i]] <- tmp_df
  }

  final <- rbind(df, do.call(rbind, out_dfs))
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

svm_support_vectors <- function(x, y, call = caller_env()) {
  rlang::check_installed("kernlab", call = call)
  model <- suppressWarnings(
    kernlab::ksvm(x = x, y = y, kernel = "rbfdot", C = 1)
  )
  sort(unique(unlist(kernlab::alphaindex(model))))
}
