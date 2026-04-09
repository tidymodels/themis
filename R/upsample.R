#' Up-Sample a Data Set Based on a Factor Variable
#'
#' `step_upsample()` creates a *specification* of a recipe step that will
#' replicate rows of a data set to make the occurrence of levels in a specific
#' factor level equal.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variable is used to sample the data. See [recipes::selections]
#'  for more details. The selection should result in _single
#'  factor variable_. For the `tidy` method, these are not
#'  currently used.
#' @param role For new variables created by this step, what analysis role
#'  should they be assigned? Only used when `indicator_column` is not `NULL`.
#' @param column A character string of the variable name that will
#'  be populated (eventually) by the `...` selectors.
#' @param over_ratio A numeric value for the ratio of the
#'  minority-to-majority frequencies. The default value (1) means
#'  that all other levels are sampled up to have the same
#'  frequency as the most occurring level. A value of 0.5 would mean
#'  that the minority levels will have (at most) (approximately)
#'  half as many rows as the majority level.
#' @param ratio Deprecated argument; same as `over_ratio`.
#' @param target An integer that will be used to subsample. This
#'  should not be set by the user and will be populated by `prep`.
#' @param indicator_column A single string or `NULL` (the default). If a
#'  string is given, a logical column with that name is added to the output,
#'  marking rows added by the step (`TRUE`) vs rows from the original data
#'  (`FALSE`).
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
#' All columns in the data are sampled and returned by [recipes::juice()]
#'  and [recipes::bake()].
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
#' step <- "step_upsample"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-unsupervised
#'
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
#'   step_upsample(class, over_ratio = 0.4523) |>
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
#'   labs(title = "Without upsample")
#'
#' recipe(class ~ x + y, data = circle_example) |>
#'   step_upsample(class) |>
#'   prep() |>
#'   bake(new_data = NULL) |>
#'   ggplot(aes(x, y, color = class)) +
#'   geom_jitter(width = 0.1, height = 0.1) +
#'   labs(title = "With upsample (with jittering)")
step_upsample <-
  function(
    recipe,
    ...,
    over_ratio = 1,
    ratio = deprecated(),
    role = NA,
    trained = FALSE,
    column = NULL,
    target = NA,
    indicator_column = NULL,
    skip = TRUE,
    seed = sample.int(10^5, 1),
    id = rand_id("upsample")
  ) {
    if (lifecycle::is_present(ratio)) {
      lifecycle::deprecate_stop(
        "0.2.0",
        "step_downsample(ratio = )",
        "step_downsample(over_ratio = )"
      )
    }

    check_number_whole(seed)
    check_string(indicator_column, allow_null = TRUE, allow_empty = FALSE)

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
        indicator_column = indicator_column,
        skip = skip,
        seed = seed,
        id = id,
        case_weights = NULL
      )
    )
  }

step_upsample_new <-
  function(
    terms,
    over_ratio,
    ratio,
    role,
    trained,
    column,
    target,
    indicator_column,
    skip,
    seed,
    id,
    case_weights
  ) {
    step(
      subclass = "upsample",
      terms = terms,
      over_ratio = over_ratio,
      ratio = ratio,
      role = role,
      trained = trained,
      column = column,
      target = target,
      indicator_column = indicator_column,
      skip = skip,
      id = id,
      seed = seed,
      case_weights = case_weights
    )
  }


#' @export
prep.step_upsample <- function(x, training, info = NULL, ...) {
  col_name <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$over_ratio, arg = "over_ratio", min = 0)

  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used) || is.null(wts)) {
    wts <- rep(1, nrow(training))
  }

  check_1_selected(col_name)
  check_column_factor(training, col_name)

  if (length(col_name) == 0) {
    majority <- 0
  } else {
    obs_freq <- weighted_table(training[[col_name]], as.integer(wts))
    majority <- max(obs_freq)
  }

  recipes::check_name(
    tibble(x = logical(0)),
    training,
    x,
    newname = x$indicator_column
  )

  step_upsample_new(
    terms = x$terms,
    ratio = x$ratio,
    over_ratio = x$over_ratio,
    role = x$role,
    trained = TRUE,
    column = col_name,
    target = floor(majority * x$over_ratio),
    indicator_column = x$indicator_column,
    skip = x$skip,
    id = x$id,
    seed = x$seed,
    case_weights = were_weights_used
  )
}


supsamp <- function(x, wts, num) {
  n <- nrow(x)
  if (n == 0) {
    return(x)
  }
  if (nrow(x) == num) {
    out <- x
  } else {
    # upsampling is done with replacement
    out <- x[sample(seq_len(n), max(num, n), replace = TRUE, prob = wts), ]
  }
  out
}

supsamp_with_indicator <- function(x, wts, num) {
  n <- nrow(x)
  if (n == 0) {
    return(list(data = x, is_new = logical(0)))
  }
  if (n >= num) {
    return(list(data = x, is_new = rep(FALSE, n)))
  }
  extra_idx <- sample(seq_len(n), num - n, replace = TRUE, prob = wts)
  list(
    data = rbind(x, x[extra_idx, ]),
    is_new = c(rep(FALSE, n), rep(TRUE, num - n))
  )
}

#' @export
bake.step_upsample <- function(object, new_data, ...) {
  col_names <- names(object$column)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0L) {
    # Empty selection
    return(new_data)
  }

  if (nrow(new_data) <= 1) {
    return(new_data)
  }

  if (isTRUE(object$case_weights)) {
    wts_col <- purrr::map_lgl(new_data, hardhat::is_case_weights)
    wts <- new_data[[names(which(wts_col))]]
    wts <- as.integer(wts)
  } else {
    wts <- rep(1, nrow(new_data))
  }

  if (any(is.na(new_data[[col_names]]))) {
    missing <- new_data[is.na(new_data[[col_names]]), ]
  } else {
    missing <- NULL
  }
  split_data <- split(new_data, new_data[[col_names]])
  split_wts <- split(wts, new_data[[col_names]])

  # Upsample with seed for reproducibility
  if (!is.null(object$indicator_column)) {
    with_seed(
      seed = object$seed,
      code = {
        result_list <- purrr::map2(
          split_data,
          split_wts,
          supsamp_with_indicator,
          num = object$target
        )
        new_data <- purrr::map(result_list, "data") |> purrr::list_rbind()
        is_new <- purrr::map(result_list, "is_new") |> purrr::list_c()
        if (!is.null(missing)) {
          missing_result <- supsamp_with_indicator(
            missing,
            wts = rep(1, nrow(missing)),
            num = object$target
          )
          new_data <- bind_rows(new_data, missing_result$data)
          is_new <- c(is_new, missing_result$is_new)
        }
      }
    )
    new_data[[object$indicator_column]] <- is_new
  } else {
    with_seed(
      seed = object$seed,
      code = {
        new_data <- purrr::map2(
          split_data,
          split_wts,
          supsamp,
          num = object$target
        ) |>
          purrr::list_rbind()
        if (!is.null(missing)) {
          new_data <- bind_rows(
            new_data,
            supsamp(missing, wts = rep(1, nrow(missing)), num = object$target)
          )
        }
      }
    )
  }

  new_data
}

#' @export
print.step_upsample <-
  function(x, width = max(20, options()$width - 26), ...) {
    title <- "Up-sampling based on "
    print_step(
      x$column,
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

#' @rdname step_upsample
#' @usage NULL
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

#' @export
#' @rdname tunable_themis
tunable.step_upsample <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio")
    ),
    source = "recipe",
    component = "step_upsample",
    component_id = x$id
  )
}

#' @rdname required_pkgs.step
#' @export
required_pkgs.step_upsample <- function(x, ...) {
  c("themis")
}
