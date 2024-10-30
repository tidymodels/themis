# bad args

    Code
      smotenc(matrix())
    Condition
      Error in `smotenc()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      smotenc(circle_example, var = "class", k = 0)
    Condition
      Error in `smotenc()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      smotenc(circle_example, var = "class", k = 5.5)
    Condition
      Error in `smotenc()`:
      ! `k` must be a whole number, not the number 5.5.

---

    Code
      smotenc(circle_example, var = "class", over_ratio = TRUE)
    Condition
      Error in `smotenc()`:
      ! `over_ratio` must be a number, not `TRUE`.

