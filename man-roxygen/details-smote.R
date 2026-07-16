#' @details
#' SMOTE generates new examples of the minority class using nearest neighbors
#' of these cases. For each existing minority class example, new examples are
#' created by interpolating between the example and its nearest neighbors. The
#' number of nearest neighbors used is controlled by the number of neighbors
#' argument (`k` in [smote()], `neighbors` in [step_smote()]), and the number
#' of new examples generated is controlled by `over_ratio`.
