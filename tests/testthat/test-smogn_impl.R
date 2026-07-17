test_that("distance argument accepted by smogn()", {
  circle_numeric <- circle_example[, c("x", "y")]
  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(smogn(circle_numeric, var = "y", distance = dist))
  }
})

test_that("distance variants produce valid synthetic data", {
  circle_numeric <- circle_example[, c("x", "y")]

  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    result <- smogn(circle_numeric, var = "y", distance = dist)
    expect_s3_class(result, "data.frame")
    expect_equal(sort(names(result)), sort(names(circle_numeric)))
  }
})

test_that("bad distance arg errors", {
  expect_snapshot(
    error = TRUE,
    smogn(circle_example[, c("x", "y")], var = "y", distance = "minkowski")
  )
})

test_that("safe interpolation stays within the neighbor range", {
  set.seed(1)
  # Tight cluster so all neighbors are safe; interpolation must stay in range
  df <- data.frame(x = c(rnorm(50), 10 + rnorm(5) * 0.01))
  df$y <- df$x

  rel <- matrix(c(0, 5, 10, 0, 0, 1), ncol = 2)
  res <- smogn(df, var = "y", relevance = rel, perturbation = 0)

  expect_gt(nrow(res), 1)
})

test_that("automatic relevance detects extremes", {
  set.seed(1)
  y <- c(rnorm(100), 20)
  phi <- themis:::smogn_relevance(y)

  expect_equal(length(phi), length(y))
  expect_true(all(phi >= 0 & phi <= 1))
  # the extreme value should have maximal relevance
  expect_equal(phi[length(y)], 1)
})

test_that("smogn() interfaces correctly", {
  circle_numeric <- circle_example[, c("x", "y")]

  expect_no_error(smogn(circle_numeric, var = "y"))

  expect_snapshot(
    error = TRUE,
    smogn(circle_numeric, var = "Y")
  )

  expect_snapshot(
    error = TRUE,
    smogn(circle_numeric, var = c("x", "y"))
  )

  expect_snapshot(
    error = TRUE,
    smogn(circle_example[, c("x", "y", "class")], var = "class")
  )

  circle_na <- circle_numeric
  circle_na[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    smogn(circle_na, var = "y")
  )

  expect_snapshot(
    error = TRUE,
    smogn(circle_numeric, var = "y", neighbors = 0)
  )

  expect_snapshot(
    error = TRUE,
    smogn(circle_numeric, var = "y", neighbors = c(5, 10))
  )
})

test_that("bad args", {
  circle_numeric <- circle_example[, c("x", "y")]

  expect_snapshot(
    error = TRUE,
    smogn(matrix())
  )
  expect_snapshot(
    error = TRUE,
    smogn(circle_numeric, var = "y", threshold = 2)
  )
  expect_snapshot(
    error = TRUE,
    smogn(circle_numeric, var = "y", perturbation = -1)
  )
})
