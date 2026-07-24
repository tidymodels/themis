test_that("instance_hardness() removes the hardest-to-classify majority points", {
  df <- data.frame(
    x = c(0, 1, 2, 3, 4, 0.5, 1.5, 2.5),
    y = rep(0, 8),
    class = factor(c(rep("maj", 5), rep("min", 3)))
  )
  # Majority points x = 1, 2 sit amongst minority points, giving them the
  # highest hardness; under_ratio = 1 downsamples maj to 3, dropping those two.
  res <- instance_hardness_impl(
    df,
    var = "class",
    ignore_vars = character(),
    k = 3,
    under_ratio = 1
  )
  expect_equal(sort(res$x[res$class == "maj"]), c(0, 3, 4))
  expect_equal(sum(res$class == "min"), 3L)
})

test_that("instance_hardness() is a no-op when already balanced", {
  df <- data.frame(
    x = c(0, 1, 10, 11),
    y = rep(0, 4),
    class = factor(c("a", "a", "b", "b"))
  )
  res <- instance_hardness_impl(
    df,
    var = "class",
    ignore_vars = character(),
    k = 2,
    under_ratio = 1
  )
  expect_identical(as.data.frame(res), df)
})

test_that("instance_hardness_impl() errors when too few observations for k", {
  df <- data.frame(
    x = c(0, 1, 2, 3),
    y = rep(0, 4),
    class = factor(c("a", "a", "a", "b"))
  )
  expect_snapshot(
    error = TRUE,
    instance_hardness_impl(
      df,
      var = "class",
      ignore_vars = character(),
      k = 5,
      under_ratio = 1
    )
  )
})

test_that("distance argument accepted by instance_hardness()", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(instance_hardness(
      circle_numeric,
      var = "class",
      distance = dist
    ))
  }
})

test_that("bad distance arg errors for instance_hardness", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_snapshot(
    error = TRUE,
    instance_hardness(circle_numeric, var = "class", distance = "minkowski")
  )
})
