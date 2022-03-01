# bad data

    Code
      rec %>% step_bsmote(x) %>% prep()
    Condition
      Error in `check_column_factor()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_bsmote(class, id) %>% prep()
    Condition
      Error in `step_bsmote()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_bsmote(x) %>% prep()
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_bsmote(Job) %>% prep()
    Condition
      Error in `step_bsmote()`:
      ! Missing values are not supported. NAs found ind: Job.

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

