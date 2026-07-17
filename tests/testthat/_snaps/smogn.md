# bad distance arg for step_smogn()

    Code
      bake(prep(step_smogn(recipe(y ~ x, data = circle_example), y, distance = "L2")),
      new_data = NULL)
    Condition
      Error in `step_smogn()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", or "chebyshev", not "L2".

# bad data

    Code
      prep(step_smogn(rec, class))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! `class` should be a numeric variable.

---

    Code
      prep(step_smogn(rec, x, y))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if predictors are not numeric

    Code
      prep(step_smogn(recipe(y ~ x + class, data = circle_example), y))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `class`

# NA in data

    Code
      prep(step_smogn(recipe(y ~ x, data = df), y))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in x.

# indicator_column bad args

    Code
      step_smogn(recipe(y ~ x, data = circle_example), y, indicator_column = 1)
    Condition
      Error in `step_smogn()`:
      ! `indicator_column` must be a single string or `NULL`, not the number 1.

---

    Code
      prep(step_smogn(recipe(y ~ x, data = circle_example), y, indicator_column = "x"))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! Name collision occurred. The following variable names already exist:
      * `x`

# bad args

    Code
      prep(step_smogn(recipe(mpg ~ ., data = mtcars), mpg, threshold = 2))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! `threshold` must be a number between 0 and 1, not the number 2.

---

    Code
      prep(step_smogn(recipe(mpg ~ ., data = mtcars), mpg, neighbors = TRUE))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      prep(step_smogn(recipe(mpg ~ ., data = mtcars), mpg, perturbation = -1))
    Condition
      Error in `step_smogn()`:
      Caused by error in `prep()`:
      ! `perturbation` must be a number larger than or equal to 0, not the number -1.

---

    Code
      step_smogn(recipe(mpg ~ ., data = mtcars), mpg, seed = TRUE)
    Condition
      Error in `step_smogn()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -2])
    Condition
      Error in `step_smogn()`:
      ! The following required column is missing from `new_data`: y.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * SMOGN based on: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * SMOGN based on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Operations 
      * SMOGN based on: y

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * SMOGN based on: y | Trained

