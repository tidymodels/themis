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
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      smote(circle_example_num, var = "class", k = -1)
    Condition
      Error in `smote()`:
      ! `k` must be a whole number larger than or equal to 1, not the number -1.

---

    Code
      smote(circle_example_num, var = "class", k = c(5, 10))
    Condition
      Error in `smote()`:
      ! `k` must be a whole number, not a double vector.

# bad args

    Code
      smote(matrix())
    Condition
      Error in `smote()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      smote(circle_example, var = "class", k = 0)
    Condition
      Error in `smote()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      smote(circle_example, var = "class", k = 5.5)
    Condition
      Error in `smote()`:
      ! `k` must be a whole number, not the number 5.5.

---

    Code
      smote(circle_example, var = "class", over_ratio = TRUE)
    Condition
      Error in `smote()`:
      ! `over_ratio` must be a number, not `TRUE`.

