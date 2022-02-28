# bad data

    Code
      rec %>% step_upsample(x) %>% prep()
    Error <rlang_error>
      x should be a factor variable.

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
      
      $terms
      <list_of<quosure>>
      
      named list()
      
      $over_ratio
      [1] 1
      
      $ratio
      [1] NA
      
      $role
      [1] NA
      
      $trained
      [1] FALSE
      
      $column
      NULL
      
      $target
      [1] NA
      
      $skip
      [1] TRUE
      
      $id
      [1] "upsample_bLxDQ"
      
      $seed
      [1] 46455
      
      attr(,"class")
      [1] "step_upsample" "step"         

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
      
      $terms
      <list_of<quosure>>
      
      named list()
      
      $over_ratio
      [1] 1
      
      $ratio
      [1] NA
      
      $role
      [1] NA
      
      $trained
      [1] TRUE
      
      $column
      named character(0)
      
      $target
      [1] 0
      
      $skip
      [1] TRUE
      
      $id
      [1] "upsample_bLxDQ"
      
      $seed
      [1] 46455
      
      attr(,"class")
      [1] "step_upsample" "step"         

