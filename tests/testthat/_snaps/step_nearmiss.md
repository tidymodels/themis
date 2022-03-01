# bad data

    Code
      rec %>% step_nearmiss(x) %>% prep()
    Condition
      Error in `check_column_factor()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_nearmiss(class, id) %>% prep()
    Condition
      Error in `step_nearmiss()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_nearmiss(x) %>% prep()
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_nearmiss(Job) %>% prep()
    Condition
      Error in `step_nearmiss()`:
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
      
      NEARMISS-1 based on <none>

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
      
      NEARMISS-1 based on <none> [trained]

