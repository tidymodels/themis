tomek_impl <- function (X, Y, verbose = TRUE) {
  stopifnot(class(verbose) == "logical", all(unique(Y) %in% c(0, 1)))
  if (any(sapply(X, is.numeric) == FALSE)) {
    stop("only numeric features are allowed to compute nearest neighbors")
  }
  N <- nrow(X)
  i.1 <- which(Y == 1)
  i.0 <- which(Y == 0)
  N.1 <- length(i.1)
  N.0 <- length(i.0)
  if (N.1 == 0 | N.0 == 0) {
    if (verbose) {
      print("observations of the same class, no majority class removed using Tomek links")
    }
    return(list(X = X, Y = Y))
  }
  else {
    X.1 <- X[i.1, ]
    Y.1 <- Y[i.1]
    timeTomek <- system.time({
      nn <- RANN::nn2(data = X, query = X.1, k = 2)$nn.idx
      nn.1 <- nn[, 2]
      indexTomekLinks <- (Y[nn.1] == 0)
      id2remove <- unique(nn.1[which(indexTomekLinks ==
                                       T)])
    })
    if (any(Y[id2remove] == 1)) {
      stop("Error: class 1 removed")
    }

    id2keep <- setdiff(1:N, id2remove)
    Xred <- X[id2keep, ]
    Yred <- Y[id2keep]
    if (verbose) {
      cat("Instances removed", N - length(id2keep), ":",
          round((N - length(id2keep))/N.0 * 100, digits = 2),
          "% of 0 class ;", round((N - length(id2keep))/N *
                                    100, digits = 2), "% of training ; Time needed",
          round(as.numeric(timeTomek["elapsed"]), digits = 2),
          "\n")
    }
    return(list(X = Xred, Y = Yred, id.rm = id2remove))
  }
}
