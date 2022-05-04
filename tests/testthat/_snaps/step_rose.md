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
      
      ROSE based on class

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
      
      ROSE based on class [trained]

# bad data

    Code
      rec %>% step_rose(x) %>% prep()
    Error <rlang_error>
      `x` should be a factor variable.

---

    Code
      rec %>% step_rose(class, id) %>% prep()
    Error <rlang_error>
      The selector should select at most a single variable

# NA in response

    Code
      recipe(Status ~ Age, data = credit_data0) %>% step_rose(Status) %>% prep()
    Error <rlang_error>
      `step_rose` cannot have any missing values. NAs found ind: Status.

# only except 2 classes

    Code
      recipe(~., data = df_char) %>% step_rose(x) %>% prep()
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
      
      ROSE based on <none>

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
      
      ROSE based on <none> [trained]

