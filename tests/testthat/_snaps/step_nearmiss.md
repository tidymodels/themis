# printing

    Code
      print(rec)
    Message <cliMessage>
      
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
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * NEARMISS-1 based on: class | Trained

# bad data

    Code
      rec %>% step_nearmiss(x) %>% prep()
    Error <recipes_error_step>
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_nearmiss(class, id) %>% prep()
    Error <recipes_error_step>
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_nearmiss(x) %>% prep()
    Error <recipes_error_step>
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_nearmiss(Job) %>% prep()
    Error <recipes_error_step>
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found ind: Job.

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
      * NEARMISS-1 based on: <none>

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
      * NEARMISS-1 based on: <none> | Trained

