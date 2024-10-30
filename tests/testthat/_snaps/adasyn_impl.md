# adasyn() interfaces correctly

    Code
      adasyn(circle_example_num, var = "Class")
    Condition
      Error in `adasyn()`:
      ! `var` must be one of "x", "y", or "class", not "Class".
      i Did you mean "class"?

---

    Code
      adasyn(circle_example_num, var = c("class", "x"))
    Condition
      Error in `adasyn()`:
      ! Please select a single factor variable for `var`.

---

    Code
      adasyn(circle_example_num, var = "x")
    Condition
      Error in `adasyn()`:
      ! `x` should refer to a factor or character column, not a double vector.

---

    Code
      adasyn(circle_example0, var = "class")
    Condition
      Error in `adasyn()`:
      ! Cannot have any missing values. NAs found in x.

---

    Code
      adasyn(circle_example_num, var = "class", k = 0)
    Condition
      Error in `adasyn()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      adasyn(circle_example_num, var = "class", k = -1)
    Condition
      Error in `adasyn()`:
      ! `k` must be a whole number larger than or equal to 1, not the number -1.

---

    Code
      adasyn(circle_example_num, var = "class", k = c(5, 10))
    Condition
      Error in `adasyn()`:
      ! `k` must be a whole number, not a double vector.

# bad args

    Code
      adasyn(matrix())
    Condition
      Error in `adasyn()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      adasyn(circle_example_num, var = "class", k = 0)
    Condition
      Error in `adasyn()`:
      ! `k` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      adasyn(circle_example_num, var = "class", k = 5.5)
    Condition
      Error in `adasyn()`:
      ! `k` must be a whole number, not the number 5.5.

---

    Code
      adasyn(circle_example_num, var = "class", over_ratio = TRUE)
    Condition
      Error in `adasyn()`:
      ! `over_ratio` must be a number, not `TRUE`.

