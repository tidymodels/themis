# errors if there isn't enough data

    Code
      bake(prep(step_instance_hardness(recipe(class ~ x + y, data = tiny_data), class,
      neighbors = 5)), new_data = NULL)
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `bake()`:
      ! Not enough observations to compute 5 nearest neighbors.
      i 3 observations were found, but 6 are needed.

# bad data

    Code
      prep(step_instance_hardness(rec, x))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_instance_hardness(rec, class, id))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      prep(step_instance_hardness(recipe(~., data = df_char), x))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      prep(step_instance_hardness(recipe(Job ~ Age, data = credit_data), Job))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# distance_with errors on non-numeric column

    Code
      prep(step_instance_hardness(recipe(class ~ ., data = df_mixed), class,
      distance_with = c(x, name)))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `name`

# bad distance arg for step_instance_hardness()

    Code
      bake(prep(step_instance_hardness(recipe(class ~ x + y, data = circle_example),
      class, distance = "L2")), new_data = NULL)
    Condition
      Error in `step_instance_hardness()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", or "chebyshev", not "L2".

# bad args

    Code
      prep(step_instance_hardness(recipe(~., data = mtcars), over_ratio = "yes"))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      ! The following argument was specified but does not exist: `over_ratio`.

---

    Code
      prep(step_instance_hardness(recipe(~., data = mtcars), neighbors = TRUE))
    Condition
      Error in `step_instance_hardness()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      step_instance_hardness(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_instance_hardness()`:
      ! `seed` must be a whole number, not `TRUE`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_instance_hardness()`:
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
      * Instance hardness based on: <none>

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
      * Instance hardness based on: <none> | Trained

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
      * Instance hardness based on: class

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
      * Instance hardness based on: class | Trained

