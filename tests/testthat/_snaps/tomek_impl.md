# tomek() interfaces correctly

    Code
      tomek(circle_example_num, var = "Class")
    Condition
      Error in `tomek()`:
      ! `var` must be one of "x", "y", or "class", not "Class".
      i Did you mean "class"?

---

    Code
      tomek(circle_example_num, var = c("class", "x"))
    Condition
      Error in `tomek()`:
      ! Please select a single factor variable for `var`.

---

    Code
      tomek(circle_example_num, var = "x")
    Condition
      Error in `tomek()`:
      ! `x` should be a factor or character variable.

---

    Code
      tomek(circle_example0, var = "class")
    Condition
      Error in `tomek()`:
      ! Cannot have any missing values. NAs found ind: x.

