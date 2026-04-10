# nn_indices() errors informatively for singular mahalanobis covariance

    Code
      nn_indices(data, 1, "mahalanobis")
    Condition
      Error in `nn_indices()`:
      ! `distance = "mahalanobis"` requires more observations than predictors in each class.
      i 4 observations were found but 5 predictors are present.
      i Try a different `distance` metric or reduce the number of predictors.

