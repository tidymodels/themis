#' Under-sampling by removing points near other classes.
#'
#' `step_nearmiss` creates a *specification* of a recipe
#'  step that removes majority class instances by undersampling points
#'  in the majority class based on their distance to other points in the
#'  same class.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_downsample
#' @inheritParams step_smote
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
#' This methods retained the points form the majority classes which has the
#' smallest mean distance to the k nearest points in the other classes.
#'
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' All columns used in this step must be numeric with no missing data.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' @references Inderjeet Mani and I Zhang. knn approach to unbalanced data
#' distributions: a case study involving information extraction. In Proceedings
#' of workshop on learning from imbalanced datasets, 2003.
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
#'   step_nearmiss(Class) %>%
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
#'   labs(title = "Without NEARMISS") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' recipe(class ~ ., data = circle_example) %>%
#'   step_nearmiss(class) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With NEARMISS") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
step_nearmiss <-
  function(recipe, ..., role = NA, trained = FALSE,
           column = NULL, under_ratio = 1, neighbors = 5, skip = TRUE,
           seed = sample.int(10^5, 1),
           id = rand_id("nearmiss")) {
    add_step(
      recipe,
      step_nearmiss_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        column = column,
        under_ratio = under_ratio,
        neighbors = neighbors,
        predictors = NULL,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_nearmiss_new <-
  function(terms, role, trained, column, under_ratio, neighbors, predictors,
           skip, seed, id) {
    step(
      subclass = "nearmiss",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      under_ratio = under_ratio,
      neighbors = neighbors,
      predictors = predictors,
      skip = skip,
      id = id,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_nearmiss <- function(x, training, info = NULL, ...) {
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
    rlang::abort("`NA` values are not allowed when using `step_nearmiss`")
  }

  step_nearmiss_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    under_ratio = x$under_ratio,
    neighbors = x$neighbors,
    predictors = predictors,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

#' @export
bake.step_nearmiss <- function(object, new_data, ...) {

  ignore_vars <- setdiff(names(new_data), c(object$predictors, object$column))

  # nearmiss with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      original_levels <- levels(new_data[[object$column]])
      new_data <- nearmiss_impl(
        df = new_data,
        var = object$column,
        ignore_vars = ignore_vars,
        k = object$neighbors,
        under_ratio = object$under_ratio
      )
      new_data[[object$column]] <- factor(new_data[[object$column]],
                                          levels = original_levels)
    }
  )

  as_tibble(new_data)
}

#' @export
print.step_nearmiss <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("NEARMISS-1 based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_nearmiss
#' @param x A `step_nearmiss` object.
#' @export
tidy.step_nearmiss <- function(x, ...) {
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



#' @rdname required_pkgs.step
#' @export
required_pkgs.step_nearmiss <- function(x, ...) {
  c("themis")
}
