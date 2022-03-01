# ratio deprecation

    Code
      new_rec <- recipe(~., data = circle_example) %>% step_downsample(class, ratio = 2)
    Condition
      Error:
      ! The `ratio` argument of `step_downsample()` was deprecated in themis 0.2.0 and is now defunct.
      Please use the `under_ratio` argument instead.

# bad data

    Code
      rec %>% step_downsample(x) %>% prep()
    Condition
      Error in `check_column_factor()`:
      ! `x` should be a factor variable.

---

    Code
      rec %>% step_downsample(class, id) %>% prep()
    Condition
      Error in `prep()`:
      ! The selector should select at most a single variable

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

