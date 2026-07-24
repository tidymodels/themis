test_that("cnn() keeps every minority observation", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 0.3, 100, 101),
    y = rep(0, 6),
    class = factor(c(rep("maj", 4), rep("min", 2)))
  )
  set.seed(1)
  res <- cnn(df, var = "class")
  expect_equal(sort(res$x[res$class == "min"]), c(100, 101))
})

test_that("cnn() condenses a redundant majority cluster to a single point", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 0.3, 100, 101),
    y = rep(0, 6),
    class = factor(c(rep("maj", 4), rep("min", 2)))
  )
  set.seed(1)
  removed <- cnn_impl(df, var = "class")
  # One majority point is enough to classify the tight cluster with 1-NN, so
  # the other three are dropped and both minority points are always kept.
  expect_equal(removed, c(2, 3, 4))
  expect_equal(sum(df$class[removed] == "min"), 0L)
})

test_that("cnn_impl() returns nothing when there is no majority class", {
  df <- data.frame(
    x = c(0, 1, 2),
    y = rep(0, 3),
    class = factor(rep("a", 3))
  )
  expect_identical(cnn_impl(df, var = "class"), integer(0))
})

test_that("distance argument accepted by cnn()", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(cnn(circle_numeric, var = "class", distance = dist))
  }
})

test_that("bad distance arg errors for cnn", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_snapshot(
    error = TRUE,
    cnn(circle_numeric, var = "class", distance = "minkowski")
  )
})
