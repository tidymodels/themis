# ratio deprecation

    Code
      new_rec <- recipe(~., data = circle_example) %>% step_upsample(class, ratio = 2)
    Error <lifecycle_error_deprecated>
      The `ratio` argument of `step_downsample()` was deprecated in themis 0.2.0 and is now defunct.
      Please use the `over_ratio` argument instead.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Operations:
      
      Up-sampling based on class

---

    Code
      prep(rec, verbose = TRUE)
    Output
      oper 1 step upsample [training] 
      The retained training set is ~ 0.04 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          4
      
      Training data contained 400 data points and no missing data.
      
      Operations:
      
      Up-sampling based on class [trained]

# bad data

    Code
      rec %>% step_upsample(x) %>% prep()
    Error <rlang_error>
      `x` should be a factor variable.

---

    Code
      rec %>% step_upsample(class, id) %>% prep()
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
      
      Up-sampling based on <none>

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
      
      Up-sampling based on <none> [trained]

