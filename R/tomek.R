#' UEnder-sampling by removing Tomek’s links.
#'
#' `step_tomek` creates a *specification* of a recipe
#'  step that removes majority class instances of tomek links. Using
#'  [unbalanced::ubTomek()].
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
#' @references Tomek. Two modifications of cnn. IEEE Trans. Syst. Man Cybern.,
#'  6:769-772, 1976.
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
#'   step_tomek(Class) %>%
#'   prep()
#'
#' sort(table(juice(ds_rec)$Class, useNA = "always"))
#'
#' # since `skip` defaults to TRUE, baking the step has no effect
#' baked_okc <- bake(ds_rec, new_data = okc)
#' table(baked_okc$Class, useNA = "always")
#'
#' library(ggplot2)
#'
#' ggplot(circle_example, aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "Without Tomek") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' recipe(class ~ ., data = circle_example) %>%
#'   step_tomek(class) %>%
#'   prep() %>%
#'   juice() %>%
#'   ggplot(aes(x, y, color = class)) +
#'   geom_point() +
#'   labs(title = "With Tomek") +
#'   xlim(c(1, 15)) +
#'   ylim(c(1, 15))
#'
#' @importFrom recipes rand_id add_step ellipse_check
step_tomek <-
  function(recipe, ..., role = NA, trained = FALSE,
           column = NULL, skip = TRUE, seed = sample.int(10^5, 1),
           id = rand_id("tomek")) {

    add_step(recipe,
             step_tomek_new(
               terms = ellipse_check(...),
               role = role,
               trained = trained,
               column = column,
               skip = skip,
               seed = seed,
               id = id
             ))
  }

#' @importFrom recipes step
step_tomek_new <-
  function(terms, role, trained, column, skip, seed, id) {
    step(
      subclass = "tomek",
      terms = terms,
      role = role,
      trained = trained,
      column = column,
      skip = skip,
      id = id,
      seed = seed,
      id = id
    )
  }

#' @importFrom recipes bake prep check_type
#' @importFrom dplyr select
#' @importFrom purrr map_lgl
#' @export
prep.step_tomek <- function(x, training, info = NULL, ...) {

  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    rlang::abort("Please select a single factor variable.")
  if (!is.factor(training[[col_name]]))
    rlang::abort(paste0(col_name, " should be a factor variable."))

  check_2_levels_only(training, col_name)
  check_type(select(training, -col_name), TRUE)

  if (any(map_lgl(training, ~ any(is.na(.x)))))
    rlang::abort("`NA` values are not allowed when using `step_tomek`")

  step_tomek_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

# Turns a binary factor a variable where the majority class is coded as 0 and
# the minority as 1.
response_0_1 <- function(x) {
  ifelse(x == names(sort(table(x)))[1], 1, 0)
}
# Turns 0-1 coded variable back into factor variable
response_0_1_to_org <- function(old, new) {
  ref <- names(sort(table(old)))
  names(ref) <- c("1", "0")
  factor(unname(ref[as.character(new)]))
}

#' @importFrom tibble as_tibble tibble
#' @importFrom withr with_seed
#' @importFrom unbalanced ubTomek
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @export
bake.step_tomek <- function(object, new_data, ...) {

  # tomek with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      tomek_data <- ubTomek(X = select(new_data, -!!object$column),
                            Y = response_0_1(new_data[[object$column]]),
                            verbose = FALSE)
    }
  )

  new_data0 <- mutate(
    tomek_data$X,
    !!object$column := response_0_1_to_org(new_data[[object$column]],
                                           tomek_data$Y)
    )

  as_tibble(new_data0[names(new_data)])
}

#' @importFrom recipes printer terms_select
#' @export
print.step_tomek <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("Tomek based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_tomek
#' @param x A `step_tomek` object.
#' @importFrom generics tidy
#' @importFrom recipes sel2char is_trained
#' @export
tidy.step_tomek <- function(x, ...) {
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
