#' Apply SMOTEN algorithm
#'
#' `step_smoten()` creates a *specification* of a recipe step that generate new
#' examples of the minority class using nearest neighbors of these cases, for
#' data sets where all predictors are categorical (nominal). The Value
#' Difference Metric (VDM) is used to measure the distance between observations.
#' For each predictor, the most common category among neighbors is chosen.
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
#' @param neighbors An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#' @param seed An integer that will be used as the seed when
#' smote-ing.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @template details-smoten
#'
#' @details
#' All columns in the data are sampled and returned by [recipes::juice()]
#'  and [recipes::bake()].
#'
#' All predictor columns must be categorical (factor or character) with no
#' missing data.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' # Minimum observations
#'
#' Each minority class must have at least `neighbors + 1` observations to
#' perform the SMOTEN algorithm.
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
#' step <- "step_smoten"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer,
#'  W. P. (2002). Smote: Synthetic minority over-sampling technique.
#'  Journal of Artificial Intelligence Research, 16:321-357.
#'
#' @seealso [smoten()] for direct implementation
#' @family Steps for over-sampling
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_cat <- hpc_data[, c("class", "protocol", "day")]
#'
#' orig <- count(hpc_cat, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_cat) |>
#'   step_smoten(class) |>
#'   prep()
#'
#' training <- up_rec |>
#'   bake(new_data = NULL) |>
#'   count(class, name = "training")
#' training
#'
#' # Since `skip` defaults to TRUE, baking the step has no effect
#' baked <- up_rec |>
#'   bake(new_data = hpc_cat) |>
#'   count(class, name = "baked")
#' baked
step_smoten <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    column = NULL,
    over_ratio = 1,
    neighbors = 5,
    skip = TRUE,
    seed = sample.int(10^5, 1),
    id = rand_id("smoten")
  ) {
    check_number_whole(seed)

    add_step(
      recipe,
      step_smoten_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        over_ratio = over_ratio,
        neighbors = neighbors,
        predictors = NULL,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_smoten_new <-
  function(
    terms,
    role,
    trained,
    column,
    over_ratio,
    neighbors,
    predictors,
    skip,
    seed,
    id
  ) {
    step(
      subclass = "smoten",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      over_ratio = over_ratio,
      neighbors = neighbors,
      predictors = predictors,
      skip = skip,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_smoten <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$over_ratio, arg = "over_ratio", min = 0)
  check_number_whole(x$neighbors, arg = "neighbors", min = 1)

  check_1_selected(col_name)
  check_column_factor(training, col_name)
  warn_unused_levels(training, col_name)

  predictors <- setdiff(recipes::recipes_names_predictors(info), col_name)
  check_na(select(training, all_of(c(col_name, predictors))))
  check_all_categorical(select(training, all_of(predictors)))

  step_smoten_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    over_ratio = x$over_ratio,
    neighbors = x$neighbors,
    predictors = predictors,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

#' @export
bake.step_smoten <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)
  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  new_data <- as.data.frame(new_data)

  predictor_data <- new_data[, col_names]

  # smoten with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      synthetic_data <- smoten_impl(
        predictor_data,
        object$column,
        k = object$neighbors,
        over_ratio = object$over_ratio
      )
      synthetic_data <- as_tibble(synthetic_data)
    }
  )
  new_data <- na_splice(new_data, synthetic_data, object)

  new_data
}

#' @export
print.step_smoten <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "SMOTEN based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_smoten
#' @usage NULL
#' @export
tidy.step_smoten <- function(x, ...) {
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
tunable.step_smoten <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio", "neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio"),
      list(pkg = "dials", fun = "neighbors", range = c(1, 10))
    ),
    source = "recipe",
    component = "step_smoten",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_smoten <- function(x, ...) {
  c("themis")
}
