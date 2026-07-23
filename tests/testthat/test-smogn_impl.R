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

test_that("degenerate outcome errors during automatic relevance", {
  expect_snapshot(error = TRUE, {
    themis:::smogn_relevance(rep(0, 100))
  })
  expect_snapshot(error = TRUE, {
    themis:::smogn_relevance(c(rep(0, 95), 1, 2, 3, 4, 5))
  })
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

test_that("single-element bins keep their own observation (#245)", {
  # Each value forms its own bin with target 1, so the keep branch subsets a
  # length-1 index that must not be treated as a `sample()` count.
  df <- data.frame(x = c(1, 2), y = c(0, 100))
  rel <- matrix(c(0, 50, 100, 0, 0, 1), ncol = 2)

  for (seed in 1:20) {
    set.seed(seed)
    res <- smogn(df, var = "y", relevance = rel, perturbation = 0)
    expect_equal(sort(res$y), c(0, 100))
  }
})

test_that("unsafe noise is scaled by sd and capped at maxD, not perturbation", {
  set.seed(1)
  # Equidistant points force every draw to be unsafe (d = sqrt(2) > maxD),
  # so the Gaussian noise sd is sd_x * min(perturbation, maxD).
  x <- diag(6)
  y <- seq_len(6)

  res <- themis:::smogn_generate(
    x,
    y,
    n_generate = 2000,
    k = 3,
    perturbation = 1e6,
    distance = "euclidean"
  )

  # With the reference formula the noise stays bounded by sd_x * maxD; the old
  # formula (perturbation * pmin(sd_x, maxD)) would blow up with perturbation.
  # Seeds are 0/1 vectors, so bounded noise keeps every value near [0, 1].
  expect_lt(max(abs(res$x)), 5)
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
