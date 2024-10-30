# smote() interfaces correctly

    Code
      smote(circle_example_num, var = "Class")
    Condition
      Error in `smote()`:
      ! `var` must be one of "x", "y", or "class", not "Class".
      i Did you mean "class"?

---

    Code
      smote(circle_example_num, var = c("class", "x"))
    Condition
      Error in `smote()`:
      ! Please select a single factor variable for `var`.

---

    Code
      smote(circle_example_num, var = "x")
    Condition
      Error in `smote()`:
      ! `x` should refer to a factor or character column, not a double vector.

---

    Code
      smote(circle_example0, var = "class")
    Condition
      Error in `smote()`:
      ! Cannot have any missing values. NAs found in x.

---

    Code
      smote(circle_example_num, var = "class", k = 0)
    Condition
      Error in `smote()`:
      ! `k` must be non-negative.

---

    Code
      smote(circle_example_num, var = "class", k = -1)
    Condition
      Error in `smote()`:
      ! `k` must be non-negative.

---

    Code
      smote(circle_example_num, var = "class", k = c(5, 10))
    Condition
      Error in `smote()`:
      ! The `k` must be length 1.

