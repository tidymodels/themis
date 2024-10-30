# ratio deprecation

    Code
      new_rec <- recipe(~., data = circle_example) %>% step_downsample(class, ratio = 2)
    Condition
      Error:
      ! The `ratio` argument of `step_downsample()` was deprecated in themis 0.2.0 and is now defunct.
      i Please use the `under_ratio` argument instead.

# bad data

    Code
      rec %>% step_downsample(x) %>% prep()
    Condition
      Error in `step_downsample()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_downsample(class, id) %>% prep()
    Condition
      Error in `step_downsample()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# case_weights

    Code
      rec1_p
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Down-sampling based on: class | Trained, weighted

---

    Code
      rec1_p
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Down-sampling based on: class | Trained, ignored weights

# bad args

    Code
      recipe(~., data = mtcars) %>% step_downsample(under_ratio = "yes") %>% prep()
    Condition
      Error in `step_downsample()`:
      Caused by error in `prep()`:
      ! `under_ratio` must be a number, not the string "yes".

---

    Code
      recipe(~., data = mtcars) %>% step_downsample(seed = TRUE)
    Condition
      Error in `step_downsample()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_downsample()`:
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
      * Down-sampling based on: <none>

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
      * Down-sampling based on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Down-sampling based on: class

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Down-sampling based on: class | Trained

