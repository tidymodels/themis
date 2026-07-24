# instance_hardness_impl() errors when too few observations for k

    Code
      instance_hardness_impl(df, var = "class", ignore_vars = character(), k = 5,
      under_ratio = 1)
    Condition
      Error:
      ! Not enough observations to compute 5 nearest neighbors.
      i 4 observations were found, but 6 are needed.

# bad distance arg errors for instance_hardness

    Code
      instance_hardness(circle_numeric, var = "class", distance = "minkowski")
    Condition
      Error in `instance_hardness()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", or "chebyshev", not "minkowski".

