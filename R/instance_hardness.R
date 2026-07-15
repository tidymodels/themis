#' Remove hard to classify points
#'
#' `step_instance_hardness()` creates a *specification* of a recipe step that
#' removes majority class instances by under-sampling the points that are
#' hardest to classify.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_downsample
#' @inheritParams step_smote
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param distance_with A call to a selector function to choose
#'  which variables are used for distance calculations. Defaults to
#'  [recipes::all_predictors()]. The variable selected by `...` is
#'  always excluded from the distance calculations.
#' @param seed An integer that will be used as the seed when
#' applied.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @details
#' The instance hardness of each observation is estimated using the
#' k-Disagreeing Neighbors measure: the proportion of the `neighbors` nearest
#' neighbors that belong to a different class. Observations that are surrounded
#' by points of a different class are considered hard to classify. For each
#' majority class, the hardest observations are removed until the desired
#' `under_ratio` is reached.
#'
#' All columns in the data are sampled and returned by [recipes::juice()]
#'  and [recipes::bake()].
#'
#' All columns selected by `distance_with` must be numeric with no missing
#' data.
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
#' step <- "step_instance_hardness"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references Smith, M. R., Martinez, T., & Giraud-Carrier, C. (2014). An
#' instance level analysis of data complexity. Machine learning, 95(2),
#' 225-256.
#'
#' @seealso [instance_hardness()] for direct implementation
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
#'   # Bring the majority levels down to about 1000 each
#'   # 1000/259 is approx 3.862
#'   step_instance_hardness(class, under_ratio = 3.862) |>
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
#' # Note that if the original data contained more rows than the
#' # target n (= ratio * majority_n), the data are left alone:
#' orig |>
#'   left_join(training, by = "class") |>
#'   left_join(baked, by = "class")
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without instance hardness") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' recipe(class ~ x + y, data = circle_example) |>
#'   step_instance_hardness(class) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With instance hardness") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
step_instance_hardness <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    column = NULL,
    under_ratio = 1,
    neighbors = 5,
    distance = "euclidean",
    skip = TRUE,
    seed = sample.int(10^5, 1),
    distance_with = recipes::all_predictors(),
    id = rand_id("instance_hardness")
  ) {
    check_number_whole(seed)
    check_distance_arg(distance)

    add_step(
      recipe,
      step_instance_hardness_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        under_ratio = under_ratio,
        neighbors = neighbors,
        distance = distance,
        predictors = NULL,
        skip = skip,
        seed = seed,
        distance_with = enquos(distance_with),
        id = id
      )
    )
  }

step_instance_hardness_new <-
  function(
    terms,
    role,
    trained,
    column,
    under_ratio,
    neighbors,
    distance,
    predictors,
    skip,
    seed,
    distance_with,
    id
  ) {
    step(
      subclass = "instance_hardness",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      under_ratio = under_ratio,
      neighbors = neighbors,
      distance = distance,
      predictors = predictors,
      skip = skip,
      seed = seed,
      distance_with = distance_with,
      id = id
    )
  }

#' @export
prep.step_instance_hardness <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$under_ratio, arg = "under_ratio", min = 0)
  check_number_whole(x$neighbors, arg = "neighbors", min = 1)

  check_1_selected(col_name)
  check_column_factor(training, col_name)

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

  step_instance_hardness_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    under_ratio = x$under_ratio,
    neighbors = x$neighbors,
    distance = x$distance,
    predictors = predictors,
    skip = x$skip,
    seed = x$seed,
    distance_with = x$distance_with,
    id = x$id
  )
}

#' @export
bake.step_instance_hardness <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  ignore_vars <- setdiff(names(new_data), col_names)

  # instance_hardness with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      original_levels <- levels(new_data[[object$column]])
      new_data <- instance_hardness_impl(
        df = new_data,
        var = object$column,
        ignore_vars = ignore_vars,
        k = object$neighbors,
        under_ratio = object$under_ratio,
        distance = object$distance
      )
      new_data[[object$column]] <- factor(
        new_data[[object$column]],
        levels = original_levels
      )
    }
  )

  new_data
}

#' @export
print.step_instance_hardness <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "Instance hardness based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_instance_hardness
#' @usage NULL
#' @export
tidy.step_instance_hardness <- function(x, ...) {
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
tunable.step_instance_hardness <- function(x, ...) {
  tibble::tibble(
    name = c("under_ratio", "neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "under_ratio"),
      list(pkg = "dials", fun = "neighbors", range = c(1, 10))
    ),
    source = "recipe",
    component = "step_instance_hardness",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_instance_hardness <- function(x, ...) {
  c("themis")
}
