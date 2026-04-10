# bad distance arg for step_bsmote()

    Code
      bake(prep(step_bsmote(recipe(class ~ x + y, data = circle_example), class,
      distance = "L2")), new_data = NULL)
    Condition
      Error in `step_bsmote()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", or "chebyshev", not "L2".

# errors if there isn't enough data

    Code
      prep(step_bsmote(recipe(Status ~ Age, data = credit_data0), Status))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `bake()`:
      ! The minority class "dummy" does not have enough danger observations to perform BSMOTE.
      i 0 danger observations were found, but 6 are needed.

# bad data

    Code
      prep(step_bsmote(rec, x))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_bsmote(rec, class, id))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      prep(step_bsmote(recipe(~., data = df_char), x))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      prep(step_bsmote(recipe(Job ~ Age, data = credit_data), Job))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# indicator_column bad args

    Code
      step_bsmote(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = 1)
    Condition
      Error in `step_bsmote()`:
      ! `indicator_column` must be a single string or `NULL`, not the number 1.

---

    Code
      prep(step_bsmote(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = ""))
    Condition
      Error in `step_bsmote()`:
      ! `indicator_column` must be a single string or `NULL`, not the empty string "".

---

    Code
      prep(step_bsmote(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = "x"))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! Name collision occurred. The following variable names already exist:
      * `x`

# bad args

    Code
      prep(step_bsmote(recipe(~., data = mtcars), over_ratio = "yes"))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      prep(step_bsmote(recipe(~., data = mtcars), neighbors = TRUE))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      prep(step_bsmote(recipe(~., data = mtcars), all_neighbors = "yes"))
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `all_neighbors` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      step_bsmote(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_bsmote()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_bsmote()`:
      ! The following required column is missing from `new_data`: class.

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
      * BorderlineSMOTE based on: <none>

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
      * BorderlineSMOTE based on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * BorderlineSMOTE based on: class

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * BorderlineSMOTE based on: class | Trained

