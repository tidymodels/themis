# errors if there isn't enough data

    Code
      prep(step_smote(recipe(Status ~ Age, data = credit_data0), Status))
    Condition
      Error in `step_smote()`:
      Caused by error in `bake()`:
      ! Not enough observations of "dummy" to perform SMOTE.

# bad data

    Code
      prep(step_smote(rec, x))
    Condition
      Error in `step_smote()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_smote(rec, class, id))
    Condition
      Error in `step_smote()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      prep(step_smote(recipe(~., data = df_char), x))
    Condition
      Error in `step_smote()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      prep(step_smote(recipe(Job ~ Age, data = credit_data), Job))
    Condition
      Error in `step_smote()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# bad args

    Code
      prep(step_smote(recipe(~., data = mtcars), over_ratio = "yes"))
    Condition
      Error in `step_smote()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      prep(step_smote(recipe(~., data = mtcars), neighbors = TRUE))
    Condition
      Error in `step_smote()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      step_smote(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_smote()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_smote()`:
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
      * SMOTE based on: <none>

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
      * SMOTE based on: <none> | Trained

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
      * SMOTE based on: class

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
      * SMOTE based on: class | Trained

