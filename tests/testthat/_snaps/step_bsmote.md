# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Operations:
      
      BorderlineSMOTE based on class

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      BorderlineSMOTE based on class [trained]

# bad data

    Code
      rec %>% step_bsmote(x) %>% prep()
    Error <recipes_error_step>
      
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_bsmote(class, id) %>% prep()
    Error <recipes_error_step>
      
      Caused by error in `prep()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_bsmote(x) %>% prep()
    Error <recipes_error_step>
      
      Caused by error in `prep()`:
      ! All columns selected for the step should be numeric.

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_bsmote(Job) %>% prep()
    Error <recipes_error_step>
      
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found ind: Job.

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      BorderlineSMOTE based on <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      BorderlineSMOTE based on <none> [trained]
