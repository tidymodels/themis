#' tunable methods for themis
#'
#' These functions define what parameters _can_ be tuned for specific steps.
#'  They also define the recommended objects from the `dials` package that can
#'  be used to generate new parameter values and other characteristics.
#' @param x A recipe step object
#' @param ... Not used.
#' @return A tibble object.
#' @keywords internal
#' @export
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

#' @export
#' @rdname tunable.step_adasyn
tunable.step_bsmote <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio", "neighbors", "all_neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio"),
      list(pkg = "dials", fun = "neighbors"),
      list(pkg = "dials", fun = "all_neighbors")
    ),
    source = "recipe",
    component = "step_bsmote",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_adasyn
tunable.step_downsample <- function(x, ...) {
  tibble::tibble(
    name = "under_ratio",
    call_info = list(
      list(pkg = "dials", fun = "under_ratio")
    ),
    source = "recipe",
    component = "step_downsample",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_adasyn
tunable.step_nearmiss <- function(x, ...) {
  tibble::tibble(
    name = c("under_ratio", "neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "under_ratio"),
      list(pkg = "dials", fun = "neighbors", range = c(1, 10))
    ),
    source = "recipe",
    component = "step_nearmiss",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_adasyn
tunable.step_rose <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio")
    ),
    source = "recipe",
    component = "step_rose",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_adasyn
tunable.step_smote <- function(x, ...) {
  tibble::tibble(
    name = c("over_ratio", "neighbors"),
    call_info = list(
      list(pkg = "dials", fun = "over_ratio"),
      list(pkg = "dials", fun = "neighbors", range = c(1, 10))
    ),
    source = "recipe",
    component = "step_smote",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_adasyn
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
