# bad distance arg for step_kmeans_smote()

    Code
      bake(prep(step_kmeans_smote(recipe(class ~ x + y, data = circle_example), class,
      distance = "L2")), new_data = NULL)
    Condition
      Error in `step_kmeans_smote()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev", "squared_chord", "matusita", "hellinger", "bhattacharyya", "canberra", "soergel", "lorentzian", "jeffreys", "topsoe", "jensen-shannon", "jensen_difference", "taneja", or "kumar-johnson", not "L2".

# num_clusters is respected

    Code
      bake(prep(step_kmeans_smote(recipe(class ~ x + y, data = circle_example), class,
      num_clusters = 1000)), new_data = NULL)
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `bake()`:
      ! Not enough distinct observations to compute 1000 clusters.
      i 400 distinct observations were found.
      i Try a smaller `num_clusters` or remove duplicated rows.

# cluster_balance_threshold filters clusters

    Code
      bake(prep(step_kmeans_smote(recipe(class ~ x + y, data = df), class,
      num_clusters = 3)), new_data = NULL)
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `bake()`:
      ! No cluster is suitable for over-sampling the minority class "a".
      i No cluster both reached `cluster_balance_threshold = 1` and contained more than 2 observations of that class.
      i Try a smaller `cluster_balance_threshold`, a smaller `num_clusters`, or fewer `neighbors`.

# bad data

    Code
      prep(step_kmeans_smote(rec, x))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `x` should be a factor variable.

---

    Code
      prep(step_kmeans_smote(rec, class, id))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! The selector should select at most a single variable.

# errors if character are present

    Code
      prep(step_kmeans_smote(recipe(~., data = df_char), x))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `y`

# NA in response

    Code
      prep(step_kmeans_smote(recipe(Job ~ Age, data = credit_data), Job))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! Cannot have any missing values. NAs found in Job.

# indicator_column bad args

    Code
      step_kmeans_smote(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = 1)
    Condition
      Error in `step_kmeans_smote()`:
      ! `indicator_column` must be a single string or `NULL`, not the number 1.

---

    Code
      prep(step_kmeans_smote(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = ""))
    Condition
      Error in `step_kmeans_smote()`:
      ! `indicator_column` must be a single string or `NULL`, not the empty string "".

---

    Code
      prep(step_kmeans_smote(recipe(class ~ x + y, data = circle_example), class,
      indicator_column = "x"))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! Name collision occurred. The following variable names already exist:
      * `x`

# bad args

    Code
      prep(step_kmeans_smote(recipe(~., data = mtcars), over_ratio = "yes"))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      prep(step_kmeans_smote(recipe(~., data = mtcars), neighbors = TRUE))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number, not `TRUE`.

---

    Code
      prep(step_kmeans_smote(recipe(~., data = mtcars), num_clusters = 1))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `num_clusters` must be a whole number larger than or equal to 2 or `NULL`, not the number 1.

---

    Code
      prep(step_kmeans_smote(recipe(~., data = mtcars), cluster_balance_threshold = "yes"))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `cluster_balance_threshold` must be a number, not the string "yes".

---

    Code
      prep(step_kmeans_smote(recipe(~., data = mtcars), density_exponent = -1))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `density_exponent` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      step_kmeans_smote(recipe(~., data = mtcars), seed = TRUE)
    Condition
      Error in `step_kmeans_smote()`:
      ! `seed` must be a whole number, not `TRUE`.

# unused outcome levels are skipped with a warning (#238)

    Code
      res <- bake(prep(step_kmeans_smote(recipe(class ~ x + y, data = circle_example),
      class)), new_data = NULL)
    Condition
      Warning in `prep()`:
      Unused factor level "unused" in `class` was dropped.
      i  Level with zero observations is skipped when computing sampling targets.

# step_kmeans_smote() errors with case weights (#243)

    Code
      bake(prep(step_kmeans_smote(recipe(class ~ ., data = df), class, skip = FALSE)),
      new_data = NULL)
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `bake()`:
      ! This step does not support case weights.
      i The case weights column `wts` must be removed before this step.

# step_kmeans_smote() checks `over_ratio` names when prepped (#323)

    Code
      prep(step_kmeans_smote(recipe(class ~ ., data = df), class, over_ratio = c(a = 1,
        potato = 1)))
    Condition
      Error in `step_kmeans_smote()`:
      Caused by error in `prep()`:
      ! `over_ratio` names must be levels of the outcome.
      x Unknown name: "potato".
      i Available levels: "a", "b", and "c".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(trained, new_data = circle_example[, -3])
    Condition
      Error in `step_kmeans_smote()`:
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
      * KMeans-SMOTE based on: <none>

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
      * KMeans-SMOTE based on: <none> | Trained

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
      * KMeans-SMOTE based on: class

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
      * KMeans-SMOTE based on: class | Trained

