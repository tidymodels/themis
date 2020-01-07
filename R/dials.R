#' Parameter to determine which neighbors to use
#'
#' Used in `step_bsmote()`.
#'
#' @param values A vector of possible values (TRUE or FALSE).
#' @examples
#' all_neighbors()
#' @export
all_neighbors <- function(values = c(TRUE, FALSE)) {
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(all_neighbors = "All Neighbors"),
    finalize = NULL
  )
}
