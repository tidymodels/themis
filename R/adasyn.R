#' Apply Adaptive Synthetic Algorithm
#'
#' `step_adasyn()` creates a *specification* of a recipe step that generates
#' synthetic positive instances using ADASYN algorithm.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_upsample
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param neighbors An integer. Number of nearest neighbor that are used
#'  to generate the new examples of the minority class.
#' @param seed An integer that will be used as the seed when
#' applied.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
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
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble is retruned with
#'  columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_adasyn"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references He, H., Bai, Y., Garcia, E. and Li, S. 2008. ADASYN: Adaptive
#'  synthetic sampling approach for imbalanced learning. Proceedings of
#'  IJCNN 2008. (IEEE World Congress on Computational Intelligence). IEEE
#'  International Joint Conference. pp.1322-1328.
#'
#' @seealso [adasyn()] for direct implementation
#' @family Steps for over-sampling
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
#'   # Bring the minority levels up to about 1000 each
#'   # 1000/2211 is approx 0.4523
#'   step_adasyn(class, over_ratio = 0.4523) |>
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
#'   labs(title = "Without ADASYN")
#'
#' recipe(class ~ x + y, data = circle_example) |>
#'   step_adasyn(class) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With ADASYN")
step_adasyn <-
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
    id = rand_id("adasyn")
  ) {
    check_number_whole(seed)

    add_step(
      recipe,
      step_adasyn_new(
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

step_adasyn_new <-
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
      subclass = "adasyn",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      over_ratio = over_ratio,
      neighbors = neighbors,
      predictors = predictors,
      skip = skip,
      id = id,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_adasyn <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$over_ratio, arg = "over_ratio", min = 0)
  check_number_whole(x$neighbors, arg = "neighbors", min = 1)

  check_1_selected(col_name)
  check_column_factor(training, col_name)

  predictors <- setdiff(get_from_info(info, "predictor"), col_name)

  check_type(training[, predictors], types = c("double", "integer"))
  check_na(select(training, all_of(c(col_name, predictors))))

  step_adasyn_new(
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
bake.step_adasyn <- function(object, new_data, ...) {
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  new_data <- as.data.frame(new_data)

  predictor_data <- new_data[, col_names]

  # adasyn with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      synthetic_data <- adasyn_impl(
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
print.step_adasyn <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "adasyn based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_adasyn
#' @usage NULL
#' @export
tidy.step_adasyn <- function(x, ...) {
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
tunable.step_adasyn <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio", "neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio"),
      list(pkg = "dials", fun = "neighbors", range = c(1, 10))
    ),
    source = "recipe",
    component = "step_adasyn",
    component_id = x$id
  )
}

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.step
#' @keywords internal
#' @export
required_pkgs.step_adasyn <- function(x, ...) {
  c("themis")
}
