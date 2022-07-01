#' Apply ROSE Algorithm
#'
#' `step_rose` creates a *specification* of a recipe
#'  step that generates sample of synthetic data by enlarging the features
#'  space of minority and majority class example. Using [ROSE::ROSE()].
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
#' @param minority_prop A numeric. Determines the of over-sampling of the
#'  minority class. Defaults to 0.5.
#' @param minority_smoothness A numeric. Shrink factor to be multiplied by the
#'  smoothing parameters to estimate the conditional kernel density of the
#'  minority class. Defaults to 1.
#' @param majority_smoothness A numeric. Shrink factor to be multiplied by the
#'  smoothing parameters to estimate the conditional kernel density of the
#'  majority class. Defaults to 1.
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
#' @references Lunardon, N., Menardi, G., and Torelli, N. (2014). ROSE: a
#'  Package for Binary Imbalanced Learning. R Jorunal, 6:82–92.
#' @references Menardi, G. and Torelli, N. (2014). Training and assessing
#'  classification rules with imbalanced data. Data Mining and Knowledge
#'  Discovery, 28:92–122.
#'
#' @family Steps for over-sampling
#'
#' @export
#' @examples
#' library(recipes)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_data0 <- hpc_data %>%
#'   mutate(class = factor(class == "VF", labels = c("not VF", "VF"))) %>%
#'   select(-protocol, -day)
#'
#' orig <- count(hpc_data0, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_data0) %>%
#'   step_rose(class) %>%
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
#'   labs(title = "Without ROSE")
#'
#' recipe(class ~ x + y, data = circle_example) %>%
#'   step_rose(class) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With ROSE")
step_rose <-
  function(recipe, ..., role = NA, trained = FALSE,
           column = NULL, over_ratio = 1, minority_prop = 0.5,
           minority_smoothness = 1, majority_smoothness = 1, skip = TRUE,
           seed = sample.int(10^5, 1), id = rand_id("rose")) {
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
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_rose_new <-
  function(terms, role, trained, column, over_ratio, minority_prop,
           minority_smoothness, majority_smoothness, predictors, skip, seed,
           id) {
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
      skip = skip,
      seed = seed,
      id = id
    )
  }

#' @export
prep.step_rose <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)
  if (length(col_name) > 1) {
    rlang::abort("The selector should select at most a single variable")
  }
  if (length(col_name) == 1) {
    check_column_factor(training, col_name)
    check_2_levels_only(training, col_name)
  }

  predictors <- setdiff(get_from_info(info, "predictor"), col_name)
  check_na(select(training, all_of(col_name)), "step_rose")

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

  if (any(is.na(new_data[[object$column]]))) {
    missing <- new_data[is.na(new_data[[object$column]]), ]
  } else {
    missing <- NULL
  }

  new_data <- as.data.frame(new_data)

  predictor_data <- new_data[, col_names]

  # rose with seed for reproducibility
  majority_size <- max(table(predictor_data[[object$column]])) * 2
  with_seed(
    seed = object$seed,
    code = {
      original_levels <- levels(predictor_data[[object$column]])
      synthetic_data <- ROSE(
        string2formula(object$column),
        predictor_data,
        N = floor(majority_size * object$over_ratio),
        p = object$minority_prop,
        hmult.majo = object$majority_smoothness,
        hmult.mino = object$minority_smoothness
      )
      synthetic_data <- synthetic_data$data
      synthetic_data[[object$column]] <- factor(
        synthetic_data[[object$column]],
        levels = original_levels
      )
      synthetic_data <- as_tibble(synthetic_data)
    }
  )
  new_data <- na_splice(new_data, synthetic_data, object)

  new_data
}

#' @export
print.step_rose <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "ROSE based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_rose` object.
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

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_rose <- function(x, ...) {
  c("themis", "ROSE")
}
