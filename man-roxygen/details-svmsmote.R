#' @details
#' SVM-SMOTE (Support Vector Machine SMOTE) works the same way as SMOTE, except
#' that instead of generating points around every point of the minority class,
#' it focuses generation near the decision boundary. A support vector machine is
#' fitted to the data and the support vectors that belong to the minority class
#' are used as the base points for generating new examples.
#'
#' For each minority support vector its nearest neighbors among all classes are
#' calculated. If all of the neighbors come from a different class the support
#' vector is labeled noise and is discarded. If more than half of the neighbors
#' come from a different class the support vector is labeled "danger" and new
#' points are interpolated between it and its minority-class neighbors. The
#' remaining support vectors are considered to be in a safe region and new points
#' are extrapolated away from their minority-class neighbors.
#'
#' The number of neighbors used for this labeling is controlled by
#' `m_neighbors`, and how far the extrapolated points are placed is controlled by
#' `out_step`.
#'
#' The support vector machine is always fitted with [kernlab::ksvm()] using a
#' radial basis function kernel (`kernel = "rbfdot"`) and a cost of `C = 1`,
#' matching the reference implementations of the method. These are not exposed
#' as arguments because the fitted model is only used to identify which minority
#' observations are support vectors, and not for prediction, so the sampling
#' results are relatively insensitive to them.
