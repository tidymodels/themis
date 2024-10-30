# bad data

    Code
      rec %>% step_nearmiss(x) %>% prep()
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_nearmiss(class, id) %>% prep()
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_nearmiss(x) %>% prep()
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_nearmiss(Job) %>% prep()
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_nearmiss(over_ratio = "yes") %>% prep()
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! The following argument was specified but do not exist: `over_ratio`.

---

    Code
      recipe(~., data = mtcars) %>% step_nearmiss(neighbors = TRUE) %>% prep()
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      recipe(~., data = mtcars) %>% step_nearmiss(seed = TRUE)
    Condition
      Error in `step_nearmiss()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_nearmiss()`:
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
      * NEARMISS-1 based on: <none>

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
      * NEARMISS-1 based on: <none> | Trained

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
      * NEARMISS-1 based on: class

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
      * NEARMISS-1 based on: class | Trained

