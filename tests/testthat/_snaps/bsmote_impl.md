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
      ! x should be a factor or character variable.

---

    Code
      bsmote(circle_example0, var = "class")
    Condition
      Error in `check_na()`:
      ! `smote` cannot have any missing values. NAs found ind: x.

---

    Code
      bsmote(circle_example_num, var = "class", k = 0)
    Condition
      Error in `bsmote()`:
      ! `k` must be non-negative.

---

    Code
      bsmote(circle_example_num, var = "class", k = -1)
    Condition
      Error in `bsmote()`:
      ! `k` must be non-negative.

---

    Code
      bsmote(circle_example_num, var = "class", k = c(5, 10))
    Condition
      Error in `bsmote()`:
      ! `k` must be length 1.

