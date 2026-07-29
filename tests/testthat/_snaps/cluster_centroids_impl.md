# errors when there are too few distinct observations

    Code
      cluster_centroids(df, "class")
    Condition
      Error in `cluster_centroids()`:
      ! Not enough distinct observations in "majority" to compute 3 clusters.
      i 1 distinct observation was found.
      i Try a smaller `under_ratio` or remove duplicated rows.

# bad args

    Code
      cluster_centroids(circle_numeric, "x")
    Condition
      Error in `cluster_centroids()`:
      ! `x` should refer to a factor or character column, not a double vector.

---

    Code
      cluster_centroids(circle_example, "class")
    Condition
      Error in `cluster_centroids()`:
      ! All columns for this function should be numeric. Non-numeric column found: `id`.

---

    Code
      cluster_centroids(circle_numeric, "class", under_ratio = "yes")
    Condition
      Error in `cluster_centroids()`:
      ! `under_ratio` must be a number, not the string "yes".

---

    Code
      cluster_centroids(circle_numeric, "class", voting = "medium")
    Condition
      Error in `cluster_centroids()`:
      ! `voting` must be one of "soft" or "hard", not "medium".

