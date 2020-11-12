#' Adaptive Synthetic Sampling Approach
#'
#' `step_adasyn` creates a *specification* of a recipe
#'  step that generates synthetic positive instances using ADASYN algorithm.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_upsample
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [selections()]
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
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' All columns used in this step must be numeric with no missing data.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' @references He, H., Bai, Y., Garcia, E. and Li, S. 2008. ADASYN: Adaptive
#'  synthetic sampling approach for imbalanced learning. Proceedings of
#'  IJCNN 2008. (IEEE World Congress on Computational Intelligence). IEEE
#'  International Joint Conference. pp.1322-1328.
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept subsampling
#' @export
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(okc)
#'
#' sort(table(okc$Class, useNA = "always"))
#'
#' ds_rec <- recipe(Class ~ age + height, data = okc) %>%
#'   step_meanimpute(all_predictors()) %>%
#'   step_adasyn(Class) %>%
#'   prep()
#'
#' sort(table(bake(ds_rec, new_data = NULL)$Class, useNA = "always"))
#'
#' # since `skip` defaults to TRUE, baking the step has no effect
#' baked_okc <- bake(ds_rec, new_data = okc)
#' table(baked_okc$Class, useNA = "always")
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without ADASYN")
#'
#' recipe(class ~ ., data = circle_example) %>%
#'   step_adasyn(class) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With ADASYN")
step_adasyn <-
  function(recipe, ..., role = NA, trained = FALSE, column = NULL,
           over_ratio = 1, neighbors = 5, skip = TRUE,
           seed = sample.int(10^5, 1), id = rand_id("adasyn")) {
    add_step(
      recipe,
      step_adasyn_new(
        terms = ellipse_check(...),
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
  function(terms, role, trained, column, over_ratio, neighbors, predictors,
           skip, seed, id) {
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
  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1) {
    rlang::abort("Please select a single factor variable.")
  }
  if (!is.factor(training[[col_name]])) {
    rlang::abort(paste0(col_name, " should be a factor variable."))
  }

  predictors <- setdiff(info$variable[info$role == "predictor"], col_name)

  check_type(training[, predictors], TRUE)

  if (any(map_lgl(training, ~ any(is.na(.x))))) {
    rlang::abort("`NA` values are not allowed when using `step_adasyn`")
  }

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
  new_data <- as.data.frame(new_data)

  predictor_data <- new_data[, unique(c(object$predictors, object$column))]

  # adasyn with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      synthetic_data <- adasyn(
        predictor_data,
        object$column,
        k = object$neighbors,
        over_ratio = object$over_ratio
      )
    }
  )
  new_data <- na_splice(new_data, synthetic_data, object)

  as_tibble(new_data)
}

#' @export
print.step_adasyn <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("adasyn based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_adasyn
#' @param x A `step_adasyn` object.
#' @export
tidy.step_adasyn <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$column)
  }
  else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = unname(term_names))
  }
  res$id <- x$id
  res
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
