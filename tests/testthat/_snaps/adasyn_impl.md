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
      ! `x` should be a factor or character variable.

---

    Code
      adasyn(circle_example0, var = "class")
    Condition
      Error in `check_na()`:
      ! `smote` cannot have any missing values. NAs found ind: x.

---

    Code
      adasyn(circle_example_num, var = "class", k = 0)
    Condition
      Error in `adasyn()`:
      ! `k` must be non-negative.

---

    Code
      adasyn(circle_example_num, var = "class", k = -1)
    Condition
      Error in `adasyn()`:
      ! `k` must be non-negative.

---

    Code
      adasyn(circle_example_num, var = "class", k = c(5, 10))
    Condition
      Error in `adasyn()`:
      ! `k` must be length 1.

