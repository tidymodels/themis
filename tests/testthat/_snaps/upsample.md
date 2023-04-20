# ratio deprecation

    Code
      new_rec <- recipe(~., data = circle_example) %>% step_upsample(class, ratio = 2)
    Error <lifecycle_error_deprecated>
      The `ratio` argument of `step_downsample()` was deprecated in themis 0.2.0 and is now defunct.
      i Please use the `over_ratio` argument instead.

# printing

    Code
      print(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * Up-sampling based on: class

---

    Code
      prep(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Up-sampling based on: class | Trained

# bad data

    Code
      rec %>% step_upsample(x) %>% prep()
    Error <recipes_error_step>
      Error in `step_upsample()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_upsample(class, id) %>% prep()
    Error <recipes_error_step>
      Error in `step_upsample()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable

# empty printing

    Code
      rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Up-sampling based on: <none>

---

    Code
      rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Up-sampling based on: <none> | Trained

# case_weights

    Code
      rec1_p
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Up-sampling based on: class | Trained, weighted

---

    Code
      rec1_p
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    4
      case_weights: 1
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * Up-sampling based on: class | Trained, ignored weights

