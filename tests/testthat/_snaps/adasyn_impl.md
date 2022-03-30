# adasyn() interfaces correctly

    Code
      adasyn(circle_example_num, var = "Class")
    Error <rlang_error>
      `var` must be one of "x", "y", or "class", not "Class".
      i Did you mean "class"?

---

    Code
      adasyn(circle_example_num, var = c("class", "x"))
    Error <rlang_error>
      Please select a single factor variable for `var`.

---

    Code
      adasyn(circle_example_num, var = "x")
    Error <rlang_error>
      `x` should be a factor or character variable.

---

    Code
      adasyn(circle_example0, var = "class")
    Error <rlang_error>
      `adasyn` cannot have any missing values. NAs found ind: x.

---

    Code
      adasyn(circle_example_num, var = "class", k = 0)
    Error <rlang_error>
      `k` must be non-negative.

---

    Code
      adasyn(circle_example_num, var = "class", k = -1)
    Error <rlang_error>
      `k` must be non-negative.

---

    Code
      adasyn(circle_example_num, var = "class", k = c(5, 10))
    Error <rlang_error>
      `k` must be length 1.

