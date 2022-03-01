# errors if there isn't enough data

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_adasyn(Status) %>% prep()
    Condition
      Error in `adasyn_impl()`:
      ! Not enough observations of 'dummy' to perform ADASYN.

# bad data

    Code
      rec %>% step_adasyn(x) %>% prep()
    Condition
      Error in `check_column_factor()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_adasyn(class, id) %>% prep()
    Condition
      Error in `step_adasyn()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_adasyn(x) %>% prep()
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_adasyn(Job) %>% prep()
    Condition
      Error in `step_adasyn()`:
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
      
      adasyn based on <none>

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
      
      adasyn based on <none> [trained]

