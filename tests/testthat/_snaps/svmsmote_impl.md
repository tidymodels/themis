# svmsmote() errors when the minority class is too small for k

    Code
      svmsmote_impl(df, var = "class", k = 5)
    Condition
      Error:
      ! The minority class "min" does not have enough observations to perform SVMSMOTE.
      i 3 observations were found, but 6 are needed.

# bad distance arg errors for svmsmote

    Code
      svmsmote(circle_numeric, var = "class", distance = "minkowski")
    Condition
      Error in `svmsmote()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", or "chebyshev", not "minkowski".

