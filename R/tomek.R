#' Remove Tomek’s Links
#'
#' `step_tomek()` creates a *specification* of a recipe step that removes
#' majority class instances of tomek links.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [selections()]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param seed An integer that will be used as the seed when
#' applied.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#'
#' @details
#' The factor variable used to balance around must only have 2 levels. All
#' other variables must be numerics with no missing data.
#'
#' A tomek link is defined as a pair of points from different classes and are
#' each others nearest neighbors.
#'
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) will be returned.
#'
#' @template case-weights-not-supported
#'
#' @references Tomek. Two modifications of cnn. IEEE Trans. Syst. Man Cybern.,
#'  6:769-772, 1976.
#'
#'@seealso [tomek()] for direct implementation
#' @family Steps for under-sampling
#'
#' @export
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_data0 <- hpc_data %>%
#'   select(-protocol, -day)
#'
#' orig <- count(hpc_data0, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_data0) %>%
#'   step_tomek(class) %>%
#'   prep()
#'
#' training <- up_rec %>%
#'   bake(new_data = NULL) %>%
#'   count(class, name = "training")
#' training
#'
#' # Since `skip` defaults to TRUE, baking the step has no effect
#' baked <- up_rec %>%
#'   bake(new_data = hpc_data0) %>%
#'   count(class, name = "baked")
#' baked
#'
#' orig %>%
#'   left_join(training, by = "class") %>%
#'   left_join(baked, by = "class")
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without Tomek") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' recipe(class ~ x + y, data = circle_example) %>%
#'   step_tomek(class) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With Tomek") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
step_tomek <-
  function(recipe, ..., role = NA, trained = FALSE,
           column = NULL, skip = TRUE, seed = sample.int(10^5, 1),
           id = rand_id("tomek")) {
    add_step(
      recipe,
      step_tomek_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        column = column,
        predictors = NULL,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_tomek_new <-
  function(terms, role, trained, column, predictors, skip, seed, id) {
    step(
      subclass = "tomek",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      predictors = predictors,
      skip = skip,
      id = id,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_tomek <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)
  if (length(col_name) > 1) {
    rlang::abort("The selector should select at most a single variable")
  }

  if (length(col_name) == 1) {
    check_column_factor(training, col_name)
  }

  predictors <- setdiff(get_from_info(info, "predictor"), col_name)

  check_type(training[, predictors], types = c("double", "integer"))
  check_na(select(training, all_of(c(col_name, predictors))))

  step_tomek_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    predictors = predictors,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

#' @export
bake.step_tomek <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  predictor_data <- new_data[, col_names]

  # tomek with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      tomek_data <- tomek_impl(
        df = predictor_data,
        var = object$column
      )
    }
  )

  if (length(tomek_data) > 0) {
    new_data <- new_data[-tomek_data, ]
  }

  new_data
}

#' @export
print.step_tomek <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "Tomek based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_tomek` object.
#' @export
tidy.step_tomek <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$column))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_tomek <- function(x, ...) {
  c("themis")
}
