#' Apply ROSE Algorithm
#'
#' `step_rose()` creates a *specification* of a recipe step that generates
#' sample of synthetic data by enlarging the features space of minority and
#' majority class example. Using [ROSE::ROSE()].
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_upsample
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param minority_prop A numeric. Determines the of over-sampling of the
#'  minority class. Defaults to 0.5.
#' @param minority_smoothness A numeric. Shrink factor to be multiplied by the
#'  smoothing parameters to estimate the conditional kernel density of the
#'  minority class. Defaults to 1.
#' @param majority_smoothness A numeric. Shrink factor to be multiplied by the
#'  smoothing parameters to estimate the conditional kernel density of the
#'  majority class. Defaults to 1.
#' @param indicator_column A single string or `NULL` (the default). If a
#'  string is given, a logical column with that name is added to the output.
#'  Because ROSE generates a fully synthetic dataset, all rows are marked
#'  `TRUE`.
#' @param seed An integer that will be used as the seed when
#' rose-ing.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @details
#' The factor variable used to balance around must only have 2 levels.
#'
#' The ROSE algorithm works by selecting an observation belonging to class k
#' and generates new examples  in its neighborhood is determined by some matrix
#' H_k. Smaller values of these arguments have the effect of shrinking the
#' entries of the corresponding smoothing matrix H_k, Shrinking would be a
#' cautious choice if there is a concern that excessively large neighborhoods
#' could lead to blur the boundaries between the regions of the feature space
#' associated with each class.
#'
#' All columns in the data are sampled and returned by [recipes::juice()]
#'  and [recipes::bake()].
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is returned with
#'  columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_rose"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Lunardon, N., Menardi, G., and Torelli, N. (2014). ROSE: a
#'  Package for Binary Imbalanced Learning. R Jorunal, 6:82–92.
#' @references Menardi, G. and Torelli, N. (2014). Training and assessing
#'  classification rules with imbalanced data. Data Mining and Knowledge
#'  Discovery, 28:92–122.
#'
#' @seealso [rose()] for direct implementation
#' @family Steps for over-sampling
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_data0 <- hpc_data |>
#'   mutate(class = factor(class == "VF", labels = c("not VF", "VF"))) |>
#'   select(-protocol, -day)
#'
#' orig <- count(hpc_data0, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_data0) |>
#'   step_rose(class) |>
#'   prep()
#'
#' training <- up_rec |>
#'   bake(new_data = NULL) |>
#'   count(class, name = "training")
#' training
#'
#' # Since `skip` defaults to TRUE, baking the step has no effect
#' baked <- up_rec |>
#'   bake(new_data = hpc_data0) |>
#'   count(class, name = "baked")
#' baked
#'
#' orig |>
#'   left_join(training, by = "class") |>
#'   left_join(baked, by = "class")
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without ROSE")
#'
#' recipe(class ~ x + y, data = circle_example) |>
#'   step_rose(class) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With ROSE")
step_rose <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    column = NULL,
    over_ratio = 1,
    minority_prop = 0.5,
    minority_smoothness = 1,
    majority_smoothness = 1,
    indicator_column = NULL,
    skip = TRUE,
    seed = sample.int(10^5, 1),
    id = rand_id("rose")
  ) {
    check_number_decimal(minority_prop, min = 0)
    check_number_decimal(minority_smoothness, min = 0)
    check_number_decimal(majority_smoothness, min = 0)
    check_number_whole(seed)
    check_string(indicator_column, allow_null = TRUE, allow_empty = FALSE)

    add_step(
      recipe,
      step_rose_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        over_ratio = over_ratio,
        minority_prop = minority_prop,
        minority_smoothness = minority_smoothness,
        majority_smoothness = majority_smoothness,
        predictors = NULL,
        indicator_column = indicator_column,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_rose_new <-
  function(
    terms,
    role,
    trained,
    column,
    over_ratio,
    minority_prop,
    minority_smoothness,
    majority_smoothness,
    predictors,
    indicator_column,
    skip,
    seed,
    id
  ) {
    step(
      subclass = "rose",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      over_ratio = over_ratio,
      minority_prop = minority_prop,
      minority_smoothness = minority_smoothness,
      majority_smoothness = majority_smoothness,
      predictors = predictors,
      indicator_column = indicator_column,
      skip = skip,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_rose <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$over_ratio, arg = "over_ratio", min = 0)

  check_1_selected(col_name)
  check_column_factor(training, col_name)
  check_2_levels_only(training, col_name)

  recipes::check_name(
    tibble(x = logical(0)),
    training,
    x,
    newname = x$indicator_column
  )

  predictors <- setdiff(recipes::recipes_names_predictors(info), col_name)
  check_na(select(training, all_of(col_name)))

  step_rose_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    over_ratio = x$over_ratio,
    minority_prop = x$minority_prop,
    minority_smoothness = x$minority_smoothness,
    majority_smoothness = x$majority_smoothness,
    predictors = predictors,
    indicator_column = x$indicator_column,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}


#' @export
bake.step_rose <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  if (any(is.na(new_data[[object$column]]))) {
    missing <- new_data[is.na(new_data[[object$column]]), ]
  } else {
    missing <- NULL
  }

  new_data <- as.data.frame(new_data)

  predictor_data <- new_data[, col_names]

  # rose with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      synthetic_data <- as_tibble(rose(
        predictor_data,
        var = object$column,
        over_ratio = object$over_ratio,
        minority_prop = object$minority_prop,
        minority_smoothness = object$minority_smoothness,
        majority_smoothness = object$majority_smoothness
      ))
    }
  )
  new_data <- na_splice(new_data, synthetic_data, object)

  if (!is.null(object$indicator_column)) {
    new_data[[object$indicator_column]] <- rep(TRUE, nrow(new_data))
  }

  new_data
}

#' @export
print.step_rose <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "ROSE based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_rose
#' @usage NULL
#' @export
tidy.step_rose <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$column))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res$id <- x$id
  res
}

#' @export
#' @rdname tunable_themis
tunable.step_rose <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio")
    ),
    source = "recipe",
    component = "step_rose",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_rose <- function(x, ...) {
  c("themis", "ROSE")
}

#' ROSE Algorithm
#'
#' A thin wrapper around [ROSE::ROSE()] that generates a synthetic balanced
#' sample by enlarging the feature space of minority and majority class
#' examples.
#'
#' @inheritParams step_rose
#' @param df A data.frame or tibble. Must have 1 factor variable with exactly
#'   2 levels and remaining numeric variables.
#' @param var Character, name of variable containing the 2-level factor
#'   variable.
#'
#' @return A data.frame or tibble, depending on type of `df`.
#' @export
#'
#' @details
#' This function is a thin wrapper around [ROSE::ROSE()]. For full details on
#' the underlying implementation, see that function's documentation.
#'
#' The factor variable used to balance around must only have 2 levels.
#'
#' The ROSE algorithm works by selecting an observation belonging to class k
#' and generates new examples in its neighborhood as determined by some matrix
#' H_k. Smaller values of `minority_smoothness` and `majority_smoothness`
#' have the effect of shrinking the entries of the corresponding smoothing
#' matrix H_k. Shrinking would be a cautious choice if there is a concern
#' that excessively large neighborhoods could lead to blurring the boundaries
#' between the regions of the feature space associated with each class.
#'
#' @references Lunardon, N., Menardi, G., and Torelli, N. (2014). ROSE: a
#'  Package for Binary Imbalanced Learning. R Jorunal, 6:82–92.
#' @references Menardi, G. and Torelli, N. (2014). Training and assessing
#'  classification rules with imbalanced data. Data Mining and Knowledge
#'  Discovery, 28:92–122.
#'
#' @seealso [step_rose()] for step function of this method
#' @family Direct Implementations
#'
#' @examples
#' rose(circle_example[, c("x", "y", "class")], var = "class")
#'
#' rose(circle_example[, c("x", "y", "class")], var = "class", over_ratio = 0.8)
rose <- function(
  df,
  var,
  over_ratio = 1,
  minority_prop = 0.5,
  minority_smoothness = 1,
  majority_smoothness = 1
) {
  check_data_frame(df)
  check_var(var, df)
  check_number_decimal(over_ratio, min = 0)
  check_number_decimal(minority_prop, min = 0)
  check_number_decimal(minority_smoothness, min = 0)
  check_number_decimal(majority_smoothness, min = 0)
  check_2_levels_only(df, var)

  majority_size <- max(table(df[[var]])) * 2
  original_levels <- levels(df[[var]])
  synthetic_data <- ROSE(
    string2formula(var),
    df,
    N = floor(majority_size * over_ratio),
    p = minority_prop,
    hmult.majo = majority_smoothness,
    hmult.mino = minority_smoothness
  )
  synthetic_data <- synthetic_data$data
  synthetic_data[[var]] <- factor(
    synthetic_data[[var]],
    levels = original_levels
  )
  if (is.data.frame(df) && !inherits(df, "tbl_df")) {
    return(synthetic_data)
  }
  as_tibble(synthetic_data)
}
