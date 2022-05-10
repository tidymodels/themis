tomke_impl <- function(df, var) {
  res <- RANN::nn2(df[names(df) != var], k = 2)$nn.idx[, 2]
  remove <- logical(nrow(df))
  outcome <- df[[var]]

  for (class in unique(outcome)) {
    target <- which(outcome == class)
    neighbor <- res[target]
    neighbor_neighbor <- res[neighbor]

    tomek <- target == neighbor_neighbor & outcome[target] != outcome[neighbor]

    tomek_links <- c(target[tomek], neighbor[tomek])
    remove[tomek_links] <- TRUE
  }

  which(remove)
}
