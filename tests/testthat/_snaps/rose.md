# bad data

    Code
      rec %>% step_rose(x) %>% prep()
    Error <recipes_error_step>
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_rose(class, id) %>% prep()
    Error <recipes_error_step>
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable

# NA in response

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_rose(Status) %>% prep()
    Error <recipes_error_step>
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found ind: Status.

# only except 2 classes

    Code
      recipe(~., data = df_char) %>% step_rose(x) %>% prep()
    Error <recipes_error_step>
      Error in `step_rose()`:
      Caused by error in `prep()`:
      ! `x` must only have 2 levels.

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
      * ROSE based on: <none>

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
      * ROSE based on: <none> | Trained

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
      * ROSE based on: class

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
      * ROSE based on: class | Trained

