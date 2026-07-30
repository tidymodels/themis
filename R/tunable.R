#' tunable methods for themis
#'
#' These functions define what parameters _can_ be tuned for specific steps.
#'  They also define the recommended objects from the `dials` package that can
#'  be used to generate new parameter values and other characteristics.
#' @param x A recipe step object
#' @param ... Not used.
#' @name tunable_themis
#' @return A tibble object.
#' @keywords internal
NULL

# A per-class ratio is a vector of targets rather than a single value, so it is
# not something `dials` can generate. Supplying one opts the step out of tuning
# that argument. `is.numeric()` matters here: `x$over_ratio` may hold a `tune()`
# call, and `names(quote(tune(id = "x")))` is not `NULL`.
drop_per_class_ratio <- function(tbl, ratio) {
  if (is.numeric(ratio) && !is.null(names(ratio))) {
    tbl <- tbl[!tbl$name %in% c("over_ratio", "under_ratio"), ]
  }
  tbl
}
