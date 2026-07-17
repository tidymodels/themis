# bad distance arg errors

    Code
      smogn(circle_example[, c("x", "y")], var = "y", distance = "minkowski")
    Condition
      Error in `smogn()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", or "chebyshev", not "minkowski".

# smogn() interfaces correctly

    Code
      smogn(circle_numeric, var = "Y")
    Condition
      Error in `smogn()`:
      ! Please select a single numeric variable for `var`.

---

    Code
      smogn(circle_numeric, var = c("x", "y"))
    Condition
      Error in `smogn()`:
      ! Please select a single numeric variable for `var`.

---

    Code
      smogn(circle_example[, c("x", "y", "class")], var = "class")
    Condition
      Error in `smogn()`:
      ! `class` should refer to a numeric column.

---

    Code
      smogn(circle_na, var = "y")
    Condition
      Error in `smogn()`:
      ! Cannot have any missing values. NAs found in x.

---

    Code
      smogn(circle_numeric, var = "y", neighbors = 0)
    Condition
      Error in `smogn()`:
      ! `neighbors` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      smogn(circle_numeric, var = "y", neighbors = c(5, 10))
    Condition
      Error in `smogn()`:
      ! `neighbors` must be a whole number, not a double vector.

# bad args

    Code
      smogn(matrix())
    Condition
      Error in `smogn()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      smogn(circle_numeric, var = "y", threshold = 2)
    Condition
      Error in `smogn()`:
      ! `threshold` must be a number between 0 and 1, not the number 2.

---

    Code
      smogn(circle_numeric, var = "y", perturbation = -1)
    Condition
      Error in `smogn()`:
      ! `perturbation` must be a number larger than or equal to 0, not the number -1.

