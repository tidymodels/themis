# errors when no cluster is suitable

    Code
      kmeans_smote(df, "class", num_clusters = 3)
    Condition
      Error in `kmeans_smote()`:
      ! No cluster is suitable for over-sampling the minority class "a".
      i No cluster both reached `cluster_balance_threshold = 1` and contained more than 2 observations of that class.
      i Try a smaller `cluster_balance_threshold`, a smaller `num_clusters`, or fewer `neighbors`.

# errors when there are too few distinct observations

    Code
      kmeans_smote(df, "class", num_clusters = 10)
    Condition
      Error in `kmeans_smote()`:
      ! Not enough distinct observations to compute 10 clusters.
      i 3 distinct observations were found.
      i Try a smaller `num_clusters` or remove duplicated rows.

# bad args

    Code
      kmeans_smote(circle_numeric, "x")
    Condition
      Error in `kmeans_smote()`:
      ! `x` should refer to a factor or character column, not a double vector.

---

    Code
      kmeans_smote(circle_example, "class")
    Condition
      Error in `kmeans_smote()`:
      ! All columns for this function should be numeric. Non-numeric column found: `id`.

---

    Code
      kmeans_smote(circle_numeric, "class", k = "yes")
    Condition
      Error in `kmeans_smote()`:
      ! `k` must be a whole number, not the string "yes".

---

    Code
      kmeans_smote(circle_numeric, "class", over_ratio = "yes")
    Condition
      Error in `kmeans_smote()`:
      ! `over_ratio` must be a number, not the string "yes".

---

    Code
      kmeans_smote(circle_numeric, "class", num_clusters = 1)
    Condition
      Error in `kmeans_smote()`:
      ! `num_clusters` must be a whole number larger than or equal to 2 or `NULL`, not the number 1.

---

    Code
      kmeans_smote(circle_numeric, "class", cluster_balance_threshold = Inf)
    Condition
      Error in `kmeans_smote()`:
      ! `cluster_balance_threshold` must be a number, not `Inf`.

---

    Code
      kmeans_smote(circle_numeric, "class", density_exponent = -1)
    Condition
      Error in `kmeans_smote()`:
      ! `density_exponent` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      kmeans_smote(circle_numeric, "class", distance = "L2")
    Condition
      Error in `kmeans_smote()`:
      ! `distance` must be one of "euclidean", "cosine", "mahalanobis", "manhattan", "chebyshev", "squared_chord", "matusita", "hellinger", "bhattacharyya", "canberra", "soergel", "lorentzian", "jeffreys", "topsoe", "jensen-shannon", "jensen_difference", "taneja", or "kumar-johnson", not "L2".

