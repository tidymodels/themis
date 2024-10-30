# bad data

    Code
      rec %>% step_bsmote(x) %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_bsmote(class, id) %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_bsmote(x) %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_bsmote(Job) %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_bsmote(over_ratio = "yes") %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_bsmote(neighbors = TRUE) %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      recipe(~., data = mtcars) %>% step_bsmote(all_neighbors = "yes") %>% prep()
    Condition
      Error in `step_bsmote()`:
      Caused by error in `prep()`:
      ! `all_neighbors` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_bsmote(seed = TRUE)
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

