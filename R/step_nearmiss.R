#' Remove Points Near Other Classes
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
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) will be returned.
#'
#' @template case-weights-not-supported
#'
#' @references Inderjeet Mani and I Zhang. knn approach to unbalanced data
#' distributions: a case study involving information extraction. In Proceedings
#' of workshop on learning from imbalanced datasets, 2003.
#'
#' @seealso [nearmiss()] for direct implementation
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
#'   # Bring the majority levels down to about 1000 each
#'   # 1000/259 is approx 3.862
#'   step_nearmiss(class, under_ratio = 3.862) %>%
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
#' # Note that if the original data contained more rows than the
#' # target n (= ratio * majority_n), the data are left alone:
#' orig %>%
#'   left_join(training, by = "class") %>%
#'   left_join(baked, by = "class")
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without NEARMISS") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' recipe(class ~ x + y, data = circle_example) %>%
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
        terms = enquos(...),
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
  col_name <- recipes_eval_select(x$terms, training, info)

  if (length(col_name) > 1) {
    rlang::abort("The selector should select at most a single variable")
  }

  if (length(col_name) == 1) {
    check_column_factor(training, col_name)
  }

  predictors <- setdiff(get_from_info(info, "predictor"), col_name)

  check_type(training[, predictors], TRUE)
  check_na(select(training, all_of(c(col_name, predictors))), "step_nearmiss")

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
  col_names <- unique(c(object$predictors, object$column))
  check_new_data(col_names, object, new_data)

  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  ignore_vars <- setdiff(names(new_data), col_names)

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
      new_data[[object$column]] <- factor(
        new_data[[object$column]],
        levels = original_levels
      )
    }
  )

  new_data
}

#' @export
print.step_nearmiss <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "NEARMISS-1 based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_nearmiss` object.
#' @export
tidy.step_nearmiss <- function(x, ...) {
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
required_pkgs.step_nearmiss <- function(x, ...) {
  c("themis")
}
