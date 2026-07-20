# errors if there isn't enough data

    Code
      prep(step_smoten(recipe(class ~ x, data = df), class, neighbors = 5))
    Condition
      Error in `step_smoten()`:
      Caused by error in `bake()`:
      ! The minority class "min" does not have enough observations to perform SMOTEN.
      i 3 observations were found, but 6 are needed.

# errors on numeric predictors

    Code
      prep(step_smoten(recipe(class ~ x, data = df), class))
    Condition
      Error in `step_smoten()`:
      Caused by error in `prep()`:
      ! All predictor columns for this function should be categorical (factor or character).  Non-categorical column found: `x`.

# bad data

    Code
      prep(step_smoten(recipe(~., data = df), class, id))
    Condition
      Error in `step_smoten()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# NA in response

    Code
      prep(step_smoten(recipe(class ~ x + y, data = df), class))
    Condition
      Error in `step_smoten()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in x.

# bad args

    Code
      prep(step_smoten(recipe(class ~ x + y, data = cat_example), over_ratio = "yes"))
    Condition
      Error in `step_smoten()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      prep(step_smoten(recipe(class ~ x + y, data = cat_example), neighbors = TRUE))
    Condition
      Error in `step_smoten()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      step_smoten(recipe(class ~ x + y, data = cat_example), seed = TRUE)
    Condition
      Error in `step_smoten()`:
      ! `seed` must be a whole number, not `TRUE`.

# unused outcome levels are skipped with a warning (#238)

    Code
      res <- bake(prep(step_smoten(recipe(class ~ x + y, data = cat_example), class)),
      new_data = NULL)
    Condition
      Warning in `prep()`:
      Unused factor level "unused" in `class` was dropped.
      i  Level with zero observations is skipped when computing sampling targets.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = cat_example[, -3])
    Condition
      Error in `step_smoten()`:
      ! The following required column is missing from `new_data`: class.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * SMOTEN based on: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * SMOTEN based on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * SMOTEN based on: class

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * SMOTEN based on: class | Trained

