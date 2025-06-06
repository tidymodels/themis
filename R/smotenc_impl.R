#' SMOTENC Algorithm
#'
#' SMOTENC generates new examples of the minority class using nearest neighbors
#' of these cases, and can handle categorical variables
#'
#' @inheritParams step_smotenc
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
#' The parameter `neighbors` controls the way the new examples are created.
#' For each currently existing minority class example X new examples will be
#' created (this is controlled by the parameter `over_ratio` as mentioned
#' above). These examples will be generated by using the information from the
#' `neighbors` nearest neighbor of each example of the minority class.
#' The parameter `neighbors` controls how many of these neighbor are used.
#
#' Columns can be numeric and categorical with no missing data.
#'
#' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer,
#'  W. P. (2002). Smote: Synthetic minority over-sampling technique.
#'  Journal of Artificial Intelligence Research, 16:321-357.
#'
#' @seealso [step_smotenc()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' circle_numeric <- circle_example[, c("x", "y", "class")]
#'
#' res <- smotenc(circle_numeric, var = "class")
#'
#' res <- smotenc(circle_numeric, var = "class", k = 10)
#'
#' res <- smotenc(circle_numeric, var = "class", over_ratio = 0.8)
smotenc <- function(df, var, k = 5, over_ratio = 1) {
  check_data_frame(df)
  check_var(var, df)
  check_number_whole(k, min = 1)
  check_number_decimal(over_ratio)

  check_na(select(df, -all_of(var)))

  smotenc_impl(df, var, k, over_ratio)
}


# Splits data and appends new minority instances
smotenc_impl <- function(df, var, k, over_ratio) {
  # split data into list names by classes
  data <- split(df, df[[var]])
  # Number of majority instances
  majority_count <- max(table(df[[var]]))
  # How many minority samples do we want in total?
  ratio_target <- majority_count * over_ratio
  # How many classes do we need to upsample (account for 2+ classes!)
  # Get the indices of those classes
  which_upsample <- which(table(df[[var]]) < ratio_target)
  # For each minorty class, determine how many more samples are needed
  samples_needed <- ratio_target - table(df[[var]])[which_upsample]
  # Just saving the names of those classes
  min_names <- names(samples_needed)

  # Create a list to save all the new minority classes
  out_dfs <- list()

  # Loop through all the minorty classes, this will only loop once if there is only one minorit class
  for (i in seq_along(samples_needed)) {
    # Extract the minority dataframe
    minority <- data[[min_names[i]]]

    # Ensure that we have more minority isntances than desired neighbors
    if (nrow(minority) <= k) {
      cli::cli_abort(
        "Not enough observations of {.var {min_names[i]}} to perform SMOTE."
      )
    }

    # Run the smote algorithm (minority data, # of neighbors, # of sampeles needed)
    out_df <- smotenc_data(minority, k = k, n_samples = samples_needed[i])
    out_dfs[[i]] <- out_df
  }

  # Bind all of the synthesized minority classes together
  final <- rbind(df, do.call(rbind, out_dfs))
  # Make sure the levels are correct for every categorial variable (needed?)
  final[[var]] <- factor(final[[var]], levels = levels(df[[var]]))
  rownames(final) <- NULL
  final
}

# Uses nearest-neighbors and interpolation to generate new instances
smotenc_data <- function(
  data,
  k,
  n_samples,
  smotenc_ids = seq_len(nrow(data))
) {
  # Turning integer values into doubles
  integer_cols <- vapply(data, is.integer, FUN.VALUE = logical(1))
  if (any(integer_cols)) {
    for (col in names(integer_cols)[integer_cols]) {
      data[[col]] <- as.double(data[[col]])
    }
  }

  numeric_cols <- vapply(data, is.numeric, FUN.VALUE = logical(1))

  # Runs a nearest neighbor search
  # outputs a matrix, each row is a minority instance and each column is a nearest neighbor
  # k is +1 because the sample is always a nearest neighbor to itself
  suppressWarnings(
    ids <- t(
      gower::gower_topn(x = data, y = data, n = k + 1, nthread = 1)$index
    )
  )

  # shuffles minority indicies and repeats that shuffling until the desired number of samples is reached
  indexes <- rep(sample(smotenc_ids), length.out = n_samples)
  # tabulates how many times each minority instance is used
  index_len <- tabulate(indexes, NROW(data))

  # Initialize matrix for newly generated samples
  out <- data[rep(smotenc_ids, length.out = n_samples), ]

  # For each new sample pick a random nearest neighbor to interpoate with (1 to k)
  sampleids <- sample.int(k, n_samples, TRUE)
  # pick distance along parameterized line between current sample and chosen nearest neighbor
  runif_ids <- stats::runif(n_samples)

  out_numeric <- as.matrix(out[numeric_cols])
  out_factors <- as.matrix(out[!numeric_cols])

  data_numeric <- as.matrix(data[numeric_cols])
  data_factors <- as.matrix(data[!numeric_cols])

  iii <- 0
  for (row_num in smotenc_ids) {
    # List indices from 1:n where n is the number of times that sample is used to generate a new sample
    # iii shifts 1:n to fill in the rows of out (e.g. 1:3, 4:6, 7:8, etc.)
    index_selection <- iii + seq_len(index_len[row_num])
    # removes itself as nearest neighbour
    id_knn <- ids[row_num, ids[row_num, ] != row_num]

    # need a total of index_len[row_num] new samples
    # calculates Xnew = X1 + t*(X1-Xnn)
    dif <- data_numeric[id_knn[sampleids[index_selection]], ] -
      data_numeric[rep(row_num, index_len[row_num]), ]
    gap <- dif * runif_ids[index_selection]
    out_numeric[index_selection, ] <- data_numeric[
      rep(row_num, index_len[row_num]),
    ] +
      gap

    # Replace categories with most frequent among nearest neighbors
    cat_to_upgrade <- data_factors[
      id_knn[sampleids[index_selection]],
      ,
      drop = FALSE
    ]

    cat_modes <- apply(cat_to_upgrade, 2, Mode)

    cat_replacement <- matrix(
      rep(cat_modes, length(index_selection)),
      nrow = length(index_selection),
      byrow = TRUE
    )

    out_factors[index_selection, ] <- cat_replacement

    iii <- iii + index_len[row_num]
  }
  vec_cbind(out_numeric, out_factors)[names(data)]
}
