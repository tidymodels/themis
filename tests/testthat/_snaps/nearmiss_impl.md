# bad distance arg errors for nearmiss

    Code
      nearmiss(circle_numeric, var = "class", distance = "minkowski")
    Condition
      Error in `nearmiss()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev", "squared_chord", "matusita", "hellinger", "bhattacharyya", "canberra", "soergel", "lorentzian", "jeffreys", "topsoe", "jensen-shannon", "jensen_difference", "taneja", or "kumar-johnson", not "minkowski".

# bad args

    Code
      nearmiss(matrix())
    Condition
      Error in `nearmiss()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      nearmiss(circle_example, var = "class")
    Condition
      Error in `nearmiss()`:
      ! All columns for this function should be numeric. Non-numeric column found: `id`.

---

    Code
      nearmiss(circle_example, var = "class", k = 0)
    Condition
      Error in `nearmiss()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      nearmiss(circle_example, var = "class", k = 5.5)
    Condition
      Error in `nearmiss()`:
      ! `k` must be a whole number, not the number 5.5.

---

    Code
      nearmiss(circle_example, var = "class", under_ratio = TRUE)
    Condition
      Error in `nearmiss()`:
      ! `under_ratio` must be a number, not `TRUE`.

---

    Code
      nearmiss(circle_example[, c("x", "y", "class")], var = "class", version = 0)
    Condition
      Error in `nearmiss()`:
      ! `version` must be a whole number between 1 and 3, not the number 0.

---

    Code
      nearmiss(circle_example[, c("x", "y", "class")], var = "class", version = 4)
    Condition
      Error in `nearmiss()`:
      ! `version` must be a whole number between 1 and 3, not the number 4.

---

    Code
      nearmiss(circle_example[, c("x", "y", "class")], var = "class", version = "2")
    Condition
      Error in `nearmiss()`:
      ! `version` must be a whole number, not the string "2".

---

    Code
      nearmiss(circle_example[, c("x", "y", "class")], var = "class",
      n_neighbors_ver3 = TRUE)
    Condition
      Error in `nearmiss()`:
      ! `n_neighbors_ver3` must be a whole number, not `TRUE`.

# nearmiss version 3 errors when n_neighbors_ver3 is too large

    Code
      nearmiss(df, var = "class", k = 1, version = 3, n_neighbors_ver3 = 5)
    Condition
      Error in `nearmiss()`:
      ! Not enough observations in "maj" to compute 5 nearest neighbors for the NearMiss-3 candidate pool.
      i 4 observations were found, but 5 are needed.
      i Lower `n_neighbors_ver3`.

