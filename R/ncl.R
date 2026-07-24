#' Neighborhood Cleaning Rule
#'
#' `step_ncl()` creates a *specification* of a recipe step that removes majority
#' class observations that are noisy or that pollute the neighborhood of
#' minority class observations.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_nearmiss
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param neighbors An integer. Number of nearest neighbor that are used to
#'  decide whether an observation is removed. Defaults to `3`, unlike the
#'  over-sampling steps which default to `5`.
#' @param threshold_clean A numeric. Majority classes are only cleaned around
#'  minority class observations when their size is greater than
#'  `threshold_clean` times the size of the minority class. Defaults to `0.5`.
#' @param seed An integer that will be used as the seed when
#' applied.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @template details-ncl
#'
#' @details
#' All variables selected by `distance_with` must be numeric with no missing
#' data.
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
#' step <- "step_ncl"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Laurikkala, J. (2001). Improving identification of difficult
#' small classes by balancing class distribution. In Conference on Artificial
#' Intelligence in Medicine in Europe (pp. 63-66). Springer.
#'
#' @seealso [ncl()] for direct implementation
#' @family Steps for under-sampling
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_data0 <- hpc_data |>
#'   select(-protocol, -day)
#'
#' orig <- count(hpc_data0, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_data0) |>
#'   step_ncl(class) |>
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
#'   labs(title = "Without NCL") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' recipe(class ~ x + y, data = circle_example) |>
#'   step_ncl(class) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With NCL") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
step_ncl <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    column = NULL,
    neighbors = 3,
    distance = "euclidean",
    threshold_clean = 0.5,
    skip = TRUE,
    seed = sample.int(10^5, 1),
    distance_with = recipes::all_predictors(),
    id = rand_id("ncl")
  ) {
    check_number_whole(seed)
    check_distance_arg(distance)

    add_step(
      recipe,
      step_ncl_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        neighbors = neighbors,
        distance = distance,
        threshold_clean = threshold_clean,
        predictors = NULL,
        skip = skip,
        seed = seed,
        distance_with = enquos(distance_with),
        id = id
      )
    )
  }

step_ncl_new <-
  function(
    terms,
    role,
    trained,
    column,
    neighbors,
    distance,
    threshold_clean,
    predictors,
    skip,
    seed,
    distance_with,
    id
  ) {
    step(
      subclass = "ncl",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      neighbors = neighbors,
      distance = distance,
      threshold_clean = threshold_clean,
      predictors = predictors,
      skip = skip,
      seed = seed,
      distance_with = distance_with,
      id = id
    )
  }

#' @export
prep.step_ncl <- function(
  x,
  training,
  info = NULL,
  ...
) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_whole(x$neighbors, arg = "neighbors", min = 1)
  check_number_decimal(x$threshold_clean, arg = "threshold_clean", min = 0)

  check_1_selected(col_name)
  check_column_factor(training, col_name)
  warn_unused_levels(training, col_name)

  distance_cols <- recipes_argument_select(
    x$distance_with,
    training,
    info,
    single = FALSE,
    arg_name = "distance_with"
  )
  predictors <- setdiff(distance_cols, col_name)

  check_type(training[, predictors], types = c("double", "integer"))
  check_na(select(training, all_of(c(col_name, predictors))))

  step_ncl_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    neighbors = x$neighbors,
    distance = x$distance,
    threshold_clean = x$threshold_clean,
    predictors = predictors,
    skip = x$skip,
    seed = x$seed,
    distance_with = x$distance_with,
    id = x$id
  )
}

#' @export
bake.step_ncl <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  predictor_data <- new_data[, col_names]

  # ncl with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      ncl_data <- ncl_impl(
        df = predictor_data,
        var = object$column,
        neighbors = object$neighbors,
        distance = object$distance,
        threshold_clean = object$threshold_clean
      )
    }
  )

  if (length(ncl_data) > 0) {
    new_data <- new_data[-ncl_data, ]
  }

  new_data
}

#' @export
print.step_ncl <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "NCL based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_ncl
#' @usage NULL
#' @export
tidy.step_ncl <- function(x, ...) {
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
tunable.step_ncl <- function(x, ...) {
  tibble::tibble(
    name = c("neighbors", "threshold_clean"),
    call_info = list(
      list(pkg = "dials", fun = "neighbors", range = c(1, 10)),
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_ncl",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_ncl <- function(x, ...) {
  c("themis")
}
