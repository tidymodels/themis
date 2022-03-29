# errors if there isn't enough data

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_smote(Status) %>% prep()
    Error <rlang_error>
      Not enough observations of 'dummy' to perform SMOTE.

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
      
      SMOTE based on class

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step smote [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      SMOTE based on class [trained]

# bad data

    Code
      rec %>% step_smote(x) %>% prep()
    Error <rlang_error>
      `x` should be a factor variable.

---

    Code
      rec %>% step_smote(class, id) %>% prep()
    Error <rlang_error>
      The selector should select at most a single variable

# errors if character are present

    Code
      recipe(~., data = df_char) %>% step_smote(x) %>% prep()
    Error <rlang_error>
      All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Job ~ Age, data = credit_data) %>% step_smote(Job) %>% prep()
    Error <rlang_error>
      `step_smote` cannot have any missing values. NAs found ind: Job.

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

