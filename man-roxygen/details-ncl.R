#' @details
#' The Neighborhood Cleaning Rule (NCL) is a cleaning method that combines two
#' passes over the data. First, it applies the Edited Nearest Neighbors rule,
#' removing majority class observations whose class differs from the majority of
#' their `neighbors` nearest neighbors. Second, for each minority class
#' observation that is itself misclassified by its neighbors, the majority class
#' observations among those neighbors are removed. Compared to Edited Nearest
#' Neighbors, this focuses the cleaning on the neighborhoods of minority class
#' observations.
#'
#' The smallest class is treated as the minority class. Only majority classes
#' larger than `threshold_clean` times the size of the minority class are
#' cleaned in the second pass.
