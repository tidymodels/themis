# bsmote() interfaces correctly

    Code
      bsmote(circle_example_num, var = "Class")
    Condition
      Error in `bsmote()`:
      ! `var` must be one of "x", "y", or "class", not "Class".
      i Did you mean "class"?

---

    Code
      bsmote(circle_example_num, var = c("class", "x"))
    Condition
      Error in `bsmote()`:
      ! Please select a single factor variable for `var`.

---

    Code
      bsmote(circle_example_num, var = "x")
    Condition
      Error in `bsmote()`:
      ! `x` should refer to a factor or character column, not a double vector.

---

    Code
      bsmote(circle_example0, var = "class")
    Condition
      Error in `bsmote()`:
      ! Cannot have any missing values. NAs found in x.

---

    Code
      bsmote(circle_example_num, var = "class", k = 0)
    Condition
      Error in `bsmote()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      bsmote(circle_example_num, var = "class", k = -1)
    Condition
      Error in `bsmote()`:
      ! `k` must be a whole number larger than or equal to 1, not the number -1.

---

    Code
      bsmote(circle_example_num, var = "class", k = c(5, 10))
    Condition
      Error in `bsmote()`:
      ! `k` must be a whole number, not a double vector.

# bad args

    Code
      bsmote(matrix())
    Condition
      Error in `bsmote()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      bsmote(circle_example_num, var = "class", k = 0)
    Condition
      Error in `bsmote()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      bsmote(circle_example_num, var = "class", k = 5.5)
    Condition
      Error in `bsmote()`:
      ! `k` must be a whole number, not the number 5.5.

---

    Code
      bsmote(circle_example_num, var = "class", over_ratio = TRUE)
    Condition
      Error in `bsmote()`:
      ! `over_ratio` must be a number, not `TRUE`.

---

    Code
      bsmote(circle_example_num, var = "class", all_neighbors = 1)
    Condition
      Error in `bsmote()`:
      ! `all_neighbors` must be `TRUE` or `FALSE`, not the number 1.

