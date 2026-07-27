#' @details
#' One-Sided Selection (OSS) is an under-sampling method that combines two
#' cleaning techniques. It first applies Condensed Nearest Neighbors (CNN) to
#' reduce the majority classes to a consistent subset that correctly classifies
#' the data using a 1-nearest-neighbor rule, discarding redundant interior
#' observations. It then applies Tomek's links to the remaining observations,
#' removing the majority class observations that form Tomek links with minority
#' class observations, cleaning the decision boundary.
#'
#' The smallest class is treated as the minority class and is always kept.
#' Because the CNN step relies on a random seed observation and a random scan
#' order, results depend on the random seed.
#'
#' With more than two classes, the Tomek's links step removes both members of a
#' majority-majority link, not only links between a majority and the minority
#' class. The binary case, the primary intended use, is unaffected.
