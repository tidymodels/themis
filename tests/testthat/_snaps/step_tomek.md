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
      
      Tomek based on class

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step tomek [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      Tomek based on class [trained]

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
      recipe(~., data = df_char) %>% step_tomek(x) %>% prep()
    Error <rlang_error>
      All columns selected for the step should be numeric

# NA in response

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_tomek(Status) %>% prep()
    Error <rlang_error>
      `step_tomek` cannot have any missing values. NAs found ind: Status.

# only except 2 classes

    Code
      recipe(~., data = df_char) %>% step_tomek(x) %>% prep()
    Error <rlang_error>
      `x` must only have 2 levels.

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

