# errors if there isn't enough data

    Code
      prep(step_nearmiss(recipe(class ~ x + y, data = tiny_minority), class))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `bake()`:
      ! Not enough observations in the other classes to compute 5 nearest neighbors for "majority".
      i 1 observation was found, but 6 are needed.

# bad data

    Code
      prep(step_nearmiss(rec, x))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_nearmiss(rec, class, id))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      prep(step_nearmiss(recipe(~., data = df_char), x))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      prep(step_nearmiss(recipe(Job ~ Age, data = credit_data), Job))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# distance_with errors on non-numeric column

    Code
      prep(step_nearmiss(recipe(class ~ ., data = df_mixed), class, distance_with = c(
        x, name)))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `name`

# bad distance arg for step_nearmiss()

    Code
      bake(prep(step_nearmiss(recipe(class ~ x + y, data = circle_example), class,
      distance = "L2")), new_data = NULL)
    Condition
      Error in `step_nearmiss()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev", "squared_chord", "matusita", "hellinger", "bhattacharyya", "canberra", "soergel", "lorentzian", "jeffreys", "topsoe", "jensen-shannon", "jensen_difference", "taneja", or "kumar-johnson", not "L2".

# printing shows the nearmiss version

    Code
      step_nearmiss(recipe(class ~ x + y, data = circle_example), class, version = 2)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * NEARMISS-2 based on: class

---

    Code
      prep(step_nearmiss(recipe(class ~ x + y, data = circle_example), class,
      version = 3))
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 400 data points and no incomplete rows.
      
      -- Operations 
      * NEARMISS-3 based on: class | Trained

# bad args

    Code
      prep(step_nearmiss(recipe(~., data = mtcars), over_ratio = "yes"))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! The following argument was specified but does not exist: `over_ratio`.

---

    Code
      prep(step_nearmiss(recipe(~., data = mtcars), neighbors = TRUE))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      step_nearmiss(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_nearmiss()`:
      ! `seed` must be a whole number, not `TRUE`.

---

    Code
      step_nearmiss(recipe(~., data = mtcars), version = 4)
    Condition
      Error in `step_nearmiss()`:
      ! `version` must be a whole number between 1 and 3, not the number 4.

---

    Code
      step_nearmiss(recipe(~., data = mtcars), n_neighbors_ver3 = 0)
    Condition
      Error in `step_nearmiss()`:
      ! `n_neighbors_ver3` must be a whole number larger than or equal to 1, not the number 0.

# unused outcome levels are skipped with a warning (#238)

    Code
      res <- bake(prep(step_nearmiss(recipe(class ~ x + y, data = circle_example),
      class)), new_data = NULL)
    Condition
      Warning in `prep()`:
      Unused factor level "unused" in `class` was dropped.
      i  Level with zero observations is skipped when computing sampling targets.

# step_nearmiss() checks `under_ratio` names when prepped (#323)

    Code
      prep(step_nearmiss(recipe(class ~ ., data = df), class, under_ratio = c(a = 1,
        potato = 1)))
    Condition
      Error in `step_nearmiss()`:
      Caused by error in `prep()`:
      ! `under_ratio` names must be levels of the outcome.
      x Unknown name: "potato".
      i Available levels: "a", "b", and "c".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_nearmiss()`:
      ! The following required column is missing from `new_data`: class.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * NEARMISS-1 based on: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * NEARMISS-1 based on: <none> | Trained

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
      * NEARMISS-1 based on: class

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
      * NEARMISS-1 based on: class | Trained

