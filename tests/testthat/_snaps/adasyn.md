# errors if there isn't enough data

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_adasyn(Status) %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `bake()`:
      ! Not enough observations of "dummy" to perform ADASYN.

# bad data

    Code
      rec %>% step_adasyn(x) %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_adasyn(class, id) %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_adasyn(x) %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_adasyn(Job) %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# bad args

    Code
      recipe(~., data = mtcars) %>% step_adasyn(over_ratio = "yes") %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_adasyn(neighbors = TRUE) %>% prep()
    Condition
      Error in `step_adasyn()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      recipe(~., data = mtcars) %>% step_adasyn(seed = TRUE)
    Condition
      Error in `step_adasyn()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_adasyn()`:
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
      * adasyn based on: <none>

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
      * adasyn based on: <none> | Trained

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
      * adasyn based on: class

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
      * adasyn based on: class | Trained

