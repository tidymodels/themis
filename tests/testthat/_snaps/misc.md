# nn_indices() errors informatively for singular mahalanobis covariance

    Code
      nn_indices(data, 1, "mahalanobis")
    Condition
      Error in `nn_indices()`:
      ! `distance = "mahalanobis"` requires more observations than predictors in each class.
      i 4 observations were found but 5 predictors are present.
      i Try a different `distance` metric or reduce the number of predictors.

# mahalanobis errors informatively for collinear predictors (#246)

    Code
      nn_indices(data, 1, "mahalanobis")
    Condition
      Error in `nn_indices()`:
      ! `distance = "mahalanobis"` requires an invertible covariance matrix, but the covariance of the predictors is singular.
      i This happens when predictors are collinear or constant, or when duplicated rows leave too few distinct observations.
      i Try a different `distance` metric or remove the redundant predictors.

---

    Code
      nn_indices(data, 1, "mahalanobis")
    Condition
      Error in `nn_indices()`:
      ! `distance = "mahalanobis"` requires an invertible covariance matrix, but the covariance of the predictors is singular.
      i This happens when predictors are collinear or constant, or when duplicated rows leave too few distinct observations.
      i Try a different `distance` metric or remove the redundant predictors.

---

    Code
      nn_indices(data, 1, "mahalanobis")
    Condition
      Error in `nn_indices()`:
      ! `distance = "mahalanobis"` requires an invertible covariance matrix, but the covariance of the predictors is singular.
      i This happens when predictors are collinear or constant, or when duplicated rows leave too few distinct observations.
      i Try a different `distance` metric or remove the redundant predictors.

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

# philentropy metrics exclude similarity measures and asymmetric ones

    Code
      check_distance_arg("intersection")
    Condition
      Error:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev", "squared_chord", "matusita", "hellinger", "bhattacharyya", "canberra", "soergel", "lorentzian", "jeffreys", "topsoe", "jensen-shannon", "jensen_difference", "taneja", or "kumar-johnson", not "intersection".

---

    Code
      check_distance_arg("kullback-leibler")
    Condition
      Error:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev", "squared_chord", "matusita", "hellinger", "bhattacharyya", "canberra", "soergel", "lorentzian", "jeffreys", "topsoe", "jensen-shannon", "jensen_difference", "taneja", or "kumar-johnson", not "kullback-leibler".

# philentropy metrics that divide by values reject zeros

    Code
      nn_indices(with_zero, 1, "jeffreys")
    Condition
      Error in `nn_indices()`:
      ! `distance = "jeffreys"` requires strictly positive predictor values.
      i Zero or negative values were found in the columns used to compute distances.
      i This metric divides by individual values, so a zero makes the distance infinite.
      i Try `distance = "jensen-shannon"` or `distance = "canberra"`, which allow zeros.

---

    Code
      nn_indices(with_zero, 1, "taneja")
    Condition
      Error in `nn_indices()`:
      ! `distance = "taneja"` requires strictly positive predictor values.
      i Zero or negative values were found in the columns used to compute distances.
      i This metric divides by individual values, so a zero makes the distance infinite.
      i Try `distance = "jensen-shannon"` or `distance = "canberra"`, which allow zeros.

---

    Code
      nn_indices(with_zero, 1, "kumar-johnson")
    Condition
      Error in `nn_indices()`:
      ! `distance = "kumar-johnson"` requires strictly positive predictor values.
      i Zero or negative values were found in the columns used to compute distances.
      i This metric divides by individual values, so a zero makes the distance infinite.
      i Try `distance = "jensen-shannon"` or `distance = "canberra"`, which allow zeros.

# philentropy metrics reject negative predictors

    Code
      nn_indices(negative, 1, "canberra")
    Condition
      Error in `nn_indices()`:
      ! `distance = "canberra"` requires non-negative predictor values.
      i Negative values were found in the columns used to compute distances.
      i Each row is treated as a probability distribution by this metric.
      i Try a different `distance` metric or rescale the predictors.

