circle_numeric <- circle_example[, c("x", "y", "class")]

test_that("basic usage", {
  res <- kmeans_smote(circle_numeric, var = "class")

  expect_named(res, names(circle_numeric))
  expect_all_equal(
    as.vector(table(res$class)),
    max(table(circle_numeric$class))
  )
})

test_that("over_ratio works", {
  res <- kmeans_smote(circle_numeric, var = "class", over_ratio = 0.5)

  expect_equal(
    sort(as.numeric(table(res$class))),
    max(table(circle_numeric$class)) * c(0.5, 1)
  )
})

test_that("classes at or above the target are untouched", {
  res <- kmeans_smote(circle_numeric, var = "class", over_ratio = 0)

  expect_equal(res, circle_numeric, ignore_attr = "row.names")
})

test_that("num_clusters works", {
  res <- kmeans_smote(circle_numeric, var = "class", num_clusters = 10)

  expect_all_equal(
    as.vector(table(res$class)),
    max(table(circle_numeric$class))
  )
})

test_that("k works", {
  res <- kmeans_smote(circle_numeric, var = "class", k = 5)

  expect_all_equal(
    as.vector(table(res$class)),
    max(table(circle_numeric$class))
  )
})

test_that("new points are only generated in minority-dominated clusters", {
  set.seed(5)
  df <- data.frame(
    x = c(rnorm(60, 0), rnorm(20, 10)),
    y = c(rnorm(60, 0), rnorm(20, 10)),
    class = factor(rep(c("majority", "minority"), times = c(60, 20)))
  )

  res <- kmeans_smote(df, "class", num_clusters = 2)
  new_points <- res[-seq_len(nrow(df)), ]

  # every synthetic point falls in the minority blob around 10, not the
  # majority blob around 0
  expect_all_true(new_points$x > 5)
})

test_that("factor levels are kept", {
  res <- kmeans_smote(circle_numeric, var = "class")

  expect_equal(levels(res$class), levels(circle_numeric$class))
})

test_that("works with a character `var`", {
  df <- circle_numeric
  df$class <- as.character(df$class)

  res <- kmeans_smote(df, "class")

  expect_s3_class(res$class, "factor")
  expect_identical(levels(res$class), c("Circle", "Rest"))
  expect_identical(sum(is.na(res$class)), 0L)
})

test_that("errors when no cluster is suitable", {
  set.seed(3)
  df <- data.frame(
    x = runif(60),
    y = runif(60),
    class = factor(rep(c("a", "b"), times = c(10, 50)))
  )

  expect_snapshot(error = TRUE, kmeans_smote(df, "class", num_clusters = 3))
})

test_that("errors when there are too few distinct observations", {
  df <- data.frame(
    x = c(rep(1, 20), 1:3),
    y = c(rep(1, 20), 1:3),
    class = factor(c(rep("majority", 20), rep("minority", 3)))
  )

  expect_snapshot(
    error = TRUE,
    kmeans_smote(df, "class", num_clusters = 10)
  )
})

test_that("allocate_quotas() sums to the requested number of points", {
  expect_identical(sum(allocate_quotas(10L, c(0.5, 0.5))), 10)
  expect_identical(sum(allocate_quotas(10L, c(0.1, 0.2, 0.7))), 10)
  # weights that would each round up on their own
  expect_identical(sum(allocate_quotas(7L, rep(1 / 3, 3))), 7)
  expect_identical(allocate_quotas(5L, c(1, 0)), c(5, 0))
})

test_that("cluster_sparsity_weights() falls back to size when sparsity is 0", {
  kept <- list(
    list(n = 2, avg_dist = 0),
    list(n = 6, avg_dist = 0)
  )

  expect_identical(cluster_sparsity_weights(kept, 2), c(0.25, 0.75))
})

test_that("bad args", {
  expect_snapshot(error = TRUE, kmeans_smote(circle_numeric, "x"))
  expect_snapshot(error = TRUE, kmeans_smote(circle_example, "class"))
  expect_snapshot(
    error = TRUE,
    kmeans_smote(circle_numeric, "class", k = "yes")
  )
  expect_snapshot(
    error = TRUE,
    kmeans_smote(circle_numeric, "class", over_ratio = "yes")
  )
  expect_snapshot(
    error = TRUE,
    kmeans_smote(circle_numeric, "class", num_clusters = 1)
  )
  expect_snapshot(
    error = TRUE,
    kmeans_smote(circle_numeric, "class", cluster_balance_threshold = Inf)
  )
  expect_snapshot(
    error = TRUE,
    kmeans_smote(circle_numeric, "class", density_exponent = -1)
  )
  expect_snapshot(
    error = TRUE,
    kmeans_smote(circle_numeric, "class", distance = "L2")
  )
})
