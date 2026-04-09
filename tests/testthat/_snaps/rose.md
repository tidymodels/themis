# bad data

    Code
      prep(step_rose(rec, x))
    Condition
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_rose(rec, class, id))
    Condition
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# NA in response

    Code
      prep(step_rose(recipe(Status ~ Age, data = credit_data0), Status))
    Condition
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Status.

# only except 2 classes

    Code
      prep(step_rose(recipe(~., data = df_char), x))
    Condition
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! The `x` must only have 2 levels.

# indicator_column bad args

    Code
      step_rose(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = 1)
    Condition
      Error in `step_rose()`:
      ! `indicator_column` must be a single string or `NULL`, not the number 1.

---

    Code
      prep(step_rose(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = ""))
    Condition
      Error in `step_rose()`:
      ! `indicator_column` must be a single string or `NULL`, not the empty string "".

---

    Code
      prep(step_rose(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = "x"))
    Condition
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! Name collision occurred. The following variable names already exist:
      * `x`

# bad args

    Code
      prep(step_rose(recipe(~., data = mtcars), over_ratio = "yes"))
    Condition
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      step_rose(recipe(~., data = mtcars), minority_prop = TRUE)
    Condition
      Error in `step_rose()`:
      ! `minority_prop` must be a number, not `TRUE`.

---

    Code
      step_rose(recipe(~., data = mtcars), minority_smoothness = TRUE)
    Condition
      Error in `step_rose()`:
      ! `minority_smoothness` must be a number, not `TRUE`.

---

    Code
      step_rose(recipe(~., data = mtcars), majority_smoothness = TRUE)
    Condition
      Error in `step_rose()`:
      ! `majority_smoothness` must be a number, not `TRUE`.

---

    Code
      step_rose(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_rose()`:
      ! `seed` must be a whole number, not `TRUE`.

# rose() bad args

    Code
      rose(matrix(), var = "class")
    Condition
      Error in `rose()`:
      ! `df` must be a data frame, not a logical matrix.

---

    Code
      rose(circle_numeric, var = c("class", "x"))
    Condition
      Error in `rose()`:
      ! Please select a single factor variable for `var`.

---

    Code
      rose(circle_numeric, var = "x")
    Condition
      Error in `rose()`:
      ! `x` should refer to a factor or character column, not a double vector.

---

    Code
      rose(circle_numeric, var = "class", over_ratio = TRUE)
    Condition
      Error in `rose()`:
      ! `over_ratio` must be a number, not `TRUE`.

---

    Code
      rose(circle_numeric, var = "class", minority_prop = TRUE)
    Condition
      Error in `rose()`:
      ! `minority_prop` must be a number, not `TRUE`.

---

    Code
      rose(circle_numeric, var = "class", minority_smoothness = TRUE)
    Condition
      Error in `rose()`:
      ! `minority_smoothness` must be a number, not `TRUE`.

---

    Code
      rose(circle_numeric, var = "class", majority_smoothness = TRUE)
    Condition
      Error in `rose()`:
      ! `majority_smoothness` must be a number, not `TRUE`.

# rose() errors on more than 2 class levels

    Code
      rose(df, var = "class")
    Condition
      Error in `rose()`:
      ! The `class` must only have 2 levels.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_rose()`:
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
      * ROSE based on: <none>

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
      * ROSE based on: <none> | Trained

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
      * ROSE based on: class

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
      * ROSE based on: class | Trained

