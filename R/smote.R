#' Apply SMOTE algorithm
#'
#' `step_smote` creates a *specification* of a recipe
#'  step that generate new examples of the  minority class using nearest
#'  neighbors of these cases.
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
#' @param seed An integer that will be used as the seed when smote-ing.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which is
#'  the variable used to sample.
#' @details
#' Down-sampling is intended to be performed on the _training_ set
#'  alone. For this reason, the default is `skip = TRUE`. It is
#'  advisable to use `prep(recipe, retain = TRUE)` when preparing
#'  the recipe; in this way [juice()] can be used to obtain the
#'  down-sampled version of the data.
#'
#' If there are missing values in the factor variable that is used
#'  to define the sampling, missing data are selected at random in
#'  the same way that the other factor levels are sampled. Missing
#'  values are not used to determine the amount of data in the
#'  minority level
#'
#' For any data with factor levels occurring with the same
#'  frequency as the minority level, all data will be retained.
#'
#' All columns in the data are sampled and returned by [juice()]
#'  and [bake()].
#'
#' Keep in mind that the location of down-sampling in the step
#'  may have effects. For example, if centering and scaling,
#'  it is not clear whether those operations should be conducted
#'  _before_ or _after_ rows are removed.
#'
#' When used in modeling, users should strongly consider using the
#'  option `skip = TRUE` so that the extra sampling is _not_
#'  conducted outside of the training set.
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept subsampling
#' @export
#' @examples
#' data(okc)
#'
#' sort(table(okc$Class, useNA = "always"))
#'
#' ds_rec <- recipe(Class ~ age + height, data = okc) %>%
#'   step_smote(Class) %>%
#'   prep()
#'
#' table(juice(ds_rec)$Class, useNA = "always")
#'
#' # since `skip` defaults to TRUE, baking the step has no effect
#' baked_okc <- bake(ds_rec, new_data = okc)
#' table(baked_okc$Class, useNA = "always")
#' @importFrom recipes rand_id add_step ellipse_check
step_smote <-
  function(recipe, ..., role = NA, trained = FALSE,
           column = NULL, skip = TRUE,
           seed = sample.int(10^5, 1), id = rand_id("smote")) {

    add_step(recipe,
             step_smote_new(
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
step_smote_new <-
  function(terms, role, trained, column, skip, seed, id) {
    step(
      subclass = "smote",
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
#' @export
prep.step_smote <- function(x, training, info = NULL, ...) {

  col_name <- terms_select(x$terms, info = info)
  if (length(col_name) != 1)
    stop("Please select a single factor variable.", call. = FALSE)
  if (!is.factor(training[[col_name]]))
    stop(col_name, " should be a factor variable.", call. = FALSE)

  check_type(select(training, -col_name), TRUE)

  step_smote_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    column = col_name,
    skip = x$skip,
    seed = x$seed,
    id = x$id
  )
}

string2formula <- function(x) {
  out <- a ~ .
  out[[2]] <- rlang::sym(x)
  out
}

#' @importFrom tibble as_tibble tibble
#' @importFrom withr with_seed
#' @export
bake.step_smote <- function(object, new_data, ...) {
  if (any(is.na(new_data[[object$column]])))
    missing <- new_data[is.na(new_data[[object$column]]),]
  else
    missing <- NULL
  split_up <- split(new_data, new_data[[object$column]])

  new_data <- as.data.frame(new_data)
  # smote with seed for reproducibility
  with_seed(
    seed = object$seed,
    code = {
      new_data <- DMwR::SMOTE(string2formula(object$column), new_data)
    }
  )

  as_tibble(new_data)
}

#' @importFrom recipes printer terms_select
print.step_smote <-
  function(x, width = max(20, options()$width - 26), ...) {
    cat("SMOTE based on ", sep = "")
    printer(x$column, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_smote
#' @param x A `step_smote` object.
#' @importFrom generics tidy
#' @importFrom recipes sel2char is_trained
#' @export
tidy.step_smote <- function(x, ...) {
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
