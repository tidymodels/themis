# tomek() interfaces correctly

    Code
      tomek(circle_example_num, var = "Class")
    Error <rlang_error>
      `var` must be one of "x", "y", or "class", not "Class".
      i Did you mean "class"?

---

    Code
      tomek(circle_example_num, var = c("class", "x"))
    Error <rlang_error>
      Please select a single factor variable for `var`.

---

    Code
      tomek(circle_example_num, var = "x")
    Error <rlang_error>
      `x` should be a factor or character variable.

---

    Code
      tomek(circle_example0, var = "class")
    Error <rlang_error>
      Cannot have any missing values. NAs found ind: x.

