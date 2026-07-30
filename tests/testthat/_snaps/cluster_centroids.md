# bad data

    Code
      prep(step_cluster_centroids(rec, x))
    Condition
      Error in `step_cluster_centroids()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_cluster_centroids(rec, class, id))
    Condition
      Error in `step_cluster_centroids()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# case weights error for soft voting

    Code
      bake(prep(step_cluster_centroids(recipe(class ~ x + y + wts, data = df_wts),
      class)), new_data = NULL)
    Condition
      Error in `step_cluster_centroids()`:
      Caused by error in `bake()`:
      ! This step does not support case weights.
      i The case weights column `wts` must be removed before this step.

# unused outcome levels are skipped with a warning

    Code
      res <- bake(prep(step_cluster_centroids(recipe(class ~ x + y, data = circle_example),
      class)), new_data = NULL)
    Condition
      Warning in `prep()`:
      Unused factor level "unused" in `class` was dropped.
      i  Level with zero observations is skipped when computing sampling targets.

# bad args

    Code
      prep(step_cluster_centroids(recipe(~., data = mtcars), under_ratio = "yes"))
    Condition
      Error in `step_cluster_centroids()`:
      Caused by error in `prep()`:
      ! `under_ratio` must be a number, not the string "yes".

---

    Code
      step_cluster_centroids(recipe(~., data = mtcars), voting = "medium")
    Condition
      Error in `step_cluster_centroids()`:
      ! `voting` must be one of "soft" or "hard", not "medium".

---

    Code
      step_cluster_centroids(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_cluster_centroids()`:
      ! `seed` must be a whole number, not `TRUE`.

# step_cluster_centroids() checks `under_ratio` names when prepped (#323)

    Code
      prep(step_cluster_centroids(recipe(class ~ ., data = df), class, under_ratio = c(
        a = 1, potato = 1)))
    Condition
      Error in `step_cluster_centroids()`:
      Caused by error in `prep()`:
      ! `under_ratio` names must be levels of the outcome.
      x Unknown name: "potato".
      i Available levels: "a", "b", and "c".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_cluster_centroids()`:
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
      * ClusterCentroids based on: <none>

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
      * ClusterCentroids based on: <none> | Trained

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
      * ClusterCentroids based on: class

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
      * ClusterCentroids based on: class | Trained

