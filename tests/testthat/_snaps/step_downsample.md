# ratio deprecation

    Code
      new_rec <- recipe(~., data = circle_example) %>% step_downsample(class, ratio = 2)
    Error <lifecycle_error_deprecated>
      The `ratio` argument of `step_downsample()` was deprecated in themis 0.2.0 and is now defunct.
      Please use the `under_ratio` argument instead.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Operations:
      
      Down-sampling based on class

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      Down-sampling based on class [trained]

# bad data

    Code
      rec %>% step_downsample(x) %>% prep()
    Error <rlang_error>
      `x` should be a factor variable.

---

    Code
      rec %>% step_downsample(class, id) %>% prep()
    Error <rlang_error>
      The selector should select at most a single variable

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
      
      Down-sampling based on <none>

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
      
      Down-sampling based on <none> [trained]

# case_weights

    Code
      rec1_p
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          4
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      Down-sampling based on class [weighted, trained]

---

    Code
      rec1_p
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          4
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      Down-sampling based on class [ignored weights, trained]

