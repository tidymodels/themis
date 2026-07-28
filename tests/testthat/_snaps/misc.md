# nn_indices() errors informatively for singular mahalanobis covariance

    Code
      nn_indices(data, 1, "mahalanobis")
    Condition
      Error in `nn_indices()`:
      ! `distance = "mahalanobis"` requires more observations than predictors in each class.
      i 4 observations were found but 5 predictors are present.
      i Try a different `distance` metric or reduce the number of predictors.

# sqrt-embedded metrics reject non-distribution predictors

    Code
      nn_indices(negative, 1, "matusita")
    Condition
      Error in `nn_indices()`:
      ! `distance = "matusita"` requires non-negative predictor values.
      i Negative values were found in the columns used to compute distances.
      i Each row is treated as a probability distribution by this metric.
      i Try a different `distance` metric or rescale the predictors.

---

    Code
      nn_indices(unnormalized, 1, "hellinger")
    Condition
      Error in `nn_indices()`:
      ! `distance = "hellinger"` requires each row to sum to 1.
      i 3 rows do not sum to 1.
      i Each row is treated as a probability distribution by this metric.
      i Try `distance = "matusita"` or `distance = "squared_chord"`, which do not require this.

---

    Code
      nn_indices(unnormalized, 1, "bhattacharyya")
    Condition
      Error in `nn_indices()`:
      ! `distance = "bhattacharyya"` requires each row to sum to 1.
      i 3 rows do not sum to 1.
      i Each row is treated as a probability distribution by this metric.
      i Try `distance = "matusita"` or `distance = "squared_chord"`, which do not require this.

