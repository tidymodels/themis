# errors if there isn't enough data

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_smote(Status) %>% prep()
    Condition
      Error in `smote_impl()`:
      ! Not enough observations of 'dummy' to perform SMOTE.

# bad data

    Code
      rec %>% step_smote(x) %>% prep()
    Condition
      Error in `step_smote()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_smote(class, id) %>% prep()
    Condition
      Error in `step_smote()`:
      ! The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_smote(x) %>% prep()
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_smote(Job) %>% prep()
    Condition
      Error in `step_smote()`:
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
      
      SMOTE based on <none>

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
      
      SMOTE based on <none> [trained]

