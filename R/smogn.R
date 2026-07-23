#' Apply SMOGN Algorithm
#'
#' `step_smogn()` creates a *specification* of a recipe step that generates new
#' examples for imbalanced regression problems using SMOGN.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_upsample
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  numeric variable_. For the `tidy` method, these are not
#'  currently used.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param threshold A number between 0 and 1. Outcome values with a relevance at
#'  or above this value are treated as rare and over-sampled. Defaults to `0.5`.
#' @param relevance A matrix of relevance control points, or `NULL` (default).
#'  When `NULL`, relevance is derived automatically from the boxplot extremes of
#'  the outcome. When supplied, the first column gives outcome values and the
#'  second column their relevance in `[0, 1]`.
#' @param neighbors An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the rare values.
#' @param perturbation A number. The magnitude of the Gaussian noise added when
#'  generating synthetic examples in unsafe regions. Defaults to `0.02`.
#' @param distance A character string specifying the distance metric used for
#'  nearest neighbor calculations. One of `"euclidean"` (default), `"cosine"`,
#'  `"mahalanobis"`, `"manhattan"`, or `"chebyshev"`.
#' @param seed An integer that will be used as the seed when
#' applying SMOGN.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @template details-smogn
#'
#' @details
#' All columns in the data are sampled and returned by [recipes::juice()]
#'  and [recipes::bake()].
#'
#' All columns used in this step must be numeric with no missing data.
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
#' step <- "step_smogn"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Branco, P., Torgo, L., and Ribeiro, R. P. (2017). SMOGN: a
#'  pre-processing approach for imbalanced regression. Proceedings of Machine
#'  Learning Research, 74:36-50.
#'
#' @seealso [smogn()] for direct implementation
#' @family Steps for over-sampling
#'
#' @export
#' @examplesIf rlang::is_installed("ggplot2")
#' library(recipes)
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x)) +
#'   geom_histogram(bins = 30) +
#'   labs(title = "Without SMOGN")
#'
#' recipe(y ~ x, data = circle_example) |>
#'   step_smogn(y) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(y)) +
#'   geom_histogram(bins = 30) +
#'   labs(title = "With SMOGN")
step_smogn <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    column = NULL,
    threshold = 0.5,
    relevance = NULL,
    neighbors = 5,
    perturbation = 0.02,
    distance = "euclidean",
    indicator_column = NULL,
    skip = TRUE,
    seed = sample.int(10^5, 1),
    id = rand_id("smogn")
  ) {
    check_number_whole(seed)
    check_string(indicator_column, allow_null = TRUE, allow_empty = FALSE)
    check_distance_arg(distance)

    add_step(
      recipe,
      step_smogn_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        threshold = threshold,
        relevance = relevance,
        neighbors = neighbors,
        perturbation = perturbation,
        distance = distance,
        predictors = NULL,
        indicator_column = indicator_column,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_smogn_new <-
  function(
    terms,
    role,
    trained,
    column,
    threshold,
    relevance,
    neighbors,
    perturbation,
    distance,
    predictors,
    indicator_column,
    skip,
    seed,
    id
  ) {
    step(
      subclass = "smogn",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      threshold = threshold,
      relevance = relevance,
      neighbors = neighbors,
      perturbation = perturbation,
      distance = distance,
      predictors = predictors,
      indicator_column = indicator_column,
      skip = skip,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_smogn <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$threshold, arg = "threshold", min = 0, max = 1)
  check_number_whole(x$neighbors, arg = "neighbors", min = 1)
  check_number_decimal(x$perturbation, arg = "perturbation", min = 0)

  check_1_selected(col_name)
  check_column_numeric(training, col_name)

  recipes::check_name(
    tibble(x = logical(0)),
    training,
    x,
    newname = x$indicator_column
  )

  predictors <- setdiff(recipes::recipes_names_predictors(info), col_name)

  check_type(training[, predictors], types = c("double", "integer"))
  check_na(select(training, all_of(c(col_name, predictors))))

  step_smogn_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    threshold = x$threshold,
    relevance = x$relevance,
    neighbors = x$neighbors,
    perturbation = x$perturbation,
    distance = x$distance,
    predictors = predictors,
    indicator_column = x$indicator_column,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

#' @export
bake.step_smogn <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  check_case_weights_not_supported(new_data)

  ignore_vars <- setdiff(names(new_data), col_names)
  new_data <- as.data.frame(new_data)

  with_seed(
    seed = object$seed,
    code = {
      new_data <- smogn_impl(
        df = new_data,
        var = object$column,
        ignore_vars = ignore_vars,
        threshold = object$threshold,
        relevance = object$relevance,
        k = object$neighbors,
        perturbation = object$perturbation,
        distance = object$distance
      )
    }
  )

  n_syn <- attr(new_data, "n_synthetic")
  attr(new_data, "n_synthetic") <- NULL
  new_data <- as_tibble(new_data)

  new_data <- add_indicator_column(
    new_data,
    nrow(new_data) - n_syn,
    object$indicator_column
  )

  new_data
}

#' @export
print.step_smogn <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "SMOGN based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_smogn
#' @usage NULL
#' @export
tidy.step_smogn <- function(x, ...) {
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
tunable.step_smogn <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "neighbors", range = c(1, 10))
    ),
    source = "recipe",
    component = "step_smogn",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_smogn <- function(x, ...) {
  c("themis")
}
