# bad args

    Code
      smoten(matrix())
    Condition
      Error in `smoten()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      smoten(circle_example, var = "class", k = 0)
    Condition
      Error in `smoten()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      smoten(circle_example, var = "class", k = 5.5)
    Condition
      Error in `smoten()`:
      ! `k` must be a whole number, not the number 5.5.

---

    Code
      smoten(circle_example, var = "class", over_ratio = TRUE)
    Condition
      Error in `smoten()`:
      ! `over_ratio` must be a number, not `TRUE`.

# errors on numeric predictors

    Code
      smoten(df, var = "class")
    Condition
      Error in `smoten()`:
      ! All predictor columns for this function should be categorical (factor or character).  Non-categorical column found: `x`.

