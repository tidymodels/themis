# bad args

    Code
      nearmiss(matrix())
    Condition
      Error in `nearmiss()`:
      ! `df` must be a data frame, not a logical matrix.

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

