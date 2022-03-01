# bad data

    Code
      rec %>% step_tomek(x) %>% prep()
    Condition
      Error in `step_tomek()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_tomek(class, id) %>% prep()
    Condition
      Error in `step_tomek()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_tomek(x) %>% prep()
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_tomek(Status) %>% prep()
    Condition
      Error in `step_tomek()`:
      ! Missing values are not supported. NAs found ind: Status.

# only except 2 classes

    Code
      recipe(~., data = df_char) %>% step_tomek(x) %>% prep()
    Condition
      Error in `step_tomek()`:
      ! `x` must only have 2 levels.

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
      
      Tomek based on <none>

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
      
      Tomek based on <none> [trained]

