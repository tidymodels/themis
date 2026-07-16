#' @details
#' BSMOTE (borderline-SMOTE) works the same way as SMOTE, except that instead
#' of generating points around every point of the minority class each point is
#' first classified into the boxes "danger" and "not". For each point the
#' nearest neighbors are calculated. If all the neighbors come from a different
#' class it is labeled noise and put into the "not" box. If more than half of
#' the neighbors come from a different class it is labeled "danger". Points are
#' generated around points labeled "danger".
#'
#' If `all_neighbors = FALSE` then points are generated between nearest
#' neighbors in its own class. If `all_neighbors = TRUE` then points are
#' generated between any nearest neighbors. See examples for visualization.
