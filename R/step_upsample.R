#' Up-Sample a Data Set Based on a Factor Variable
#'
#' `step_upsample` creates a *specification* of a recipe step that
#'  will replicate rows of a data set to make the occurrence of
#'  levels in a specific factor level equal.
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
#' @param over_ratio A numeric value for the ratio of the
#'  majority-to-minority frequencies. The default value (1) means
#'  that all other levels are sampled up to have the same
#'  frequency as the most occurring level. A value of 0.5 would mean
#'  that the minority levels will have (at most) (approximately)
#'  half as many rows than the majority level.
#' @param ratio Deprecated argument; same as `over_ratio`.
#' @param target An integer that will be used to subsample. This
#'  should not be set by the user and will be populated by `prep`.
#' @param seed An integer that will be used as the seed when upsampling.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#' @details
#' Up-sampling is intended to be performed on the _training_ set
#'  alone. For this reason, the default is `skip = TRUE`.
#'
#' If there are missing values in the factor variable that is used
#'  to define the sampling, missing data are selected at random in
#'  the same way that the other factor levels are sampled. Missing
#'  values are not used to determine the amount of data in the
#'  majority level (see example below).
#'
#' For any data with factor levels occurring with the same
#'  frequency as the majority level, all data will be retained.
#'
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) will be returned.
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
#'   select(-protocol, -day)
#'
#' orig <- count(hpc_data0, class, name = "orig")
#' orig
#'
#' up_rec <- recipe(class ~ ., data = hpc_data0) %>%
#'   # Bring the minority levels up to about 1000 each
#'   # 1000/2211 is approx 0.4523
#'   step_upsample(class, over_ratio = 0.4523) %>%
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
#'   labs(title = "Without upsample")
#'
#' recipe(class ~ x + y, data = circle_example) %>%
#'   step_upsample(class) %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   ggplot(aes(x, y, color = class)) +
#'   geom_jitter(width = 0.1, height = 0.1) +
#'   labs(title = "With upsample (with jittering)")
step_upsample <-
  function(recipe, ..., over_ratio = 1, ratio = deprecated(), role = NA,
           trained = FALSE, column = NULL, target = NA, skip = TRUE,
           seed = sample.int(10^5, 1),
           id = rand_id("upsample")) {

    if (lifecycle::is_present(ratio)) {
      lifecycle::deprecate_stop(
        "0.2.0",
        "step_downsample(ratio = )",
        "step_downsample(over_ratio = )"
      )
    }

    add_step(
      recipe,
      step_upsample_new(
        terms = enquos(...),
        over_ratio = over_ratio,
        ratio = NULL,
        role = role,
        trained = trained,
        column = column,
        target = target,
        skip = skip,
        seed = seed,
        id = id
      )
    )
  }

step_upsample_new <-
  function(terms, over_ratio, ratio, role, trained, column, target, skip, seed,
           id) {
    step(
      subclass = "upsample",
      terms = terms,
      over_ratio = over_ratio,
      ratio = ratio,
      role = role,
      trained = trained,
      column = column,
      target = target,
      skip = skip,
      id = id,
      seed = seed
    )
  }


#' @export
prep.step_upsample <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)
  if (length(col_name) > 1) {
    rlang::abort("The selector should select at most a single variable")
  }

  if (length(col_name) == 0) {
    majority <- 0
  } else {
    check_column_factor(training, col_name)
    obs_freq <- table(training[[col_name]])
    majority <- max(obs_freq)
  }

  check_na(select(training, all_of(col_name)), "step_upsample")

  step_upsample_new(
    terms = x$terms,
    ratio = x$ratio,
    over_ratio = x$over_ratio,
    role = x$role,
    trained = TRUE,
    column = col_name,
    target = floor(majority * x$over_ratio),
    skip = x$skip,
    id = x$id,
    seed = x$seed
  )
}


supsamp <- function(x, num) {
  n <- nrow(x)
  if (nrow(x) == num) {
    out <- x
  } else {
    # upsampling is done with replacement
    out <- x[sample(1:n, max(num, n), replace = TRUE), ]
  }
  out
}

#' @export
bake.step_upsample <- function(object, new_data, ...) {
  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (any(is.na(new_data[[object$column]]))) {
    missing <- new_data[is.na(new_data[[object$column]]), ]
  } else {
    missing <- NULL
  }
  split_up <- split(new_data, new_data[[object$column]])

  # Upsample with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      new_data <- map_dfr(split_up, supsamp, num = object$target)
      if (!is.null(missing)) {
        new_data <- bind_rows(new_data, supsamp(missing, object$target))
      }
    }
  )

  as_tibble(new_data)
}

#' @export
print.step_upsample <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "Up-sampling based on "
    print_step(x$column, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_upsample` object.
#' @export
tidy.step_upsample <- function(x, ...) {
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
required_pkgs.step_upsample <- function(x, ...) {
  c("themis")
}
