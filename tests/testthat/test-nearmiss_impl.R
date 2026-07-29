test_that("distance argument accepted by nearmiss()", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_no_error(nearmiss(
    circle_numeric,
    var = "class",
    distance = "euclidean"
  ))
  expect_no_error(nearmiss(circle_numeric, var = "class", distance = "cosine"))
  expect_no_error(nearmiss(
    circle_numeric,
    var = "class",
    distance = "mahalanobis"
  ))
  expect_no_error(nearmiss(
    circle_numeric,
    var = "class",
    distance = "manhattan"
  ))
  expect_no_error(nearmiss(
    circle_numeric,
    var = "class",
    distance = "chebyshev"
  ))
})

test_that("bad distance arg errors for nearmiss", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_numeric, var = "class", distance = "minkowski")
  )
})

test_that("nearmiss keeps majority points closest to minority class (k=1)", {
  df <- data.frame(
    x = c(0, 1, 3, 6, 11),
    y = rep(0, 5),
    class = factor(c("min", "min", "maj", "maj", "maj"))
  )
  result <- nearmiss(df, var = "class", k = 1)
  # Nearest minority point for each majority (nearest min is x=1):
  # x=3: dist=2, x=6: dist=5, x=11: dist=10
  # under_ratio=1 keeps 2 majority → x=3 and x=6 (remove x=11)
  expect_equal(sort(result$x[result$class == "maj"]), c(3, 6))
})

test_that("nearmiss with k=2 uses mean distance to keep closest majority points", {
  df <- data.frame(
    x = c(0, 6, 10, 4, 9, 20, 30),
    y = rep(0, 7),
    class = factor(c("min", "min", "min", "maj", "maj", "maj", "maj"))
  )
  result <- nearmiss(df, var = "class", k = 2, under_ratio = 1)
  # Mean distance to the 2 nearest minority points {0, 6, 10}:
  # x=4:  nearest min=6(2), 0(4)  → mean=3.0
  # x=9:  nearest min=10(1), 6(3) → mean=2.0
  # x=20: nearest min=10(10), 6(14) → mean=12.0
  # x=30: nearest min=10(20), 6(24) → mean=22.0
  # under_ratio=1 keeps 3 majority → remove x=30
  expect_equal(sort(result$x[result$class == "maj"]), c(4, 9, 20))
})

test_that("nearmiss keeps the closest rows regardless of row order (#236)", {
  df <- data.frame(
    x = c(0, -100, 10, 1, 11, 2, 3),
    y = rep(0, 7),
    class = factor(c("min", "min", "maj", "maj", "maj", "maj", "maj"))
  )
  result <- nearmiss(df, var = "class", k = 1, under_ratio = 1)
  # Nearest minority point is x=0, so distance is |x|. under_ratio=1 keeps the
  # 2 closest majority points {1, 2}, not the positionally-selected rows.
  expect_equal(sort(result$x[result$class == "maj"]), c(1, 2))
})

test_that("nearmiss version 2 uses mean distance to the farthest points", {
  df <- data.frame(
    x = c(0, 1, 2, 10, 11),
    y = rep(0, 5),
    class = factor(c("maj", "maj", "maj", "min", "min"))
  )
  result <- nearmiss(df, var = "class", k = 1, version = 2)
  # Distance to the farthest minority point (x=11):
  # x=0: 11, x=1: 10, x=2: 9. under_ratio=1 keeps the 2 smallest → x=1 and x=2
  expect_equal(sort(result$x[result$class == "maj"]), c(1, 2))
})

test_that("nearmiss version 3 keeps the farthest points of the candidate pool", {
  df <- data.frame(
    x = c(0, 10, 1, 2, 3, 11, 12),
    y = rep(0, 7),
    class = factor(c("min", "min", rep("maj", 5)))
  )
  result <- nearmiss(
    df,
    var = "class",
    k = 1,
    version = 3,
    n_neighbors_ver3 = 1
  )
  # Pool = nearest majority point of each minority point → x=1 (for 0) and
  # x=11 (for 10). Both fit within the target of 2, so both are kept.
  expect_equal(sort(result$x[result$class == "maj"]), c(1, 11))
})

test_that("nearmiss version 3 pool grows with n_neighbors_ver3", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  small <- nearmiss(circle_numeric, var = "class", version = 3)
  large <- nearmiss(
    circle_numeric,
    var = "class",
    version = 3,
    n_neighbors_ver3 = 10
  )
  expect_gt(nrow(large), nrow(small))
})

test_that("nearmiss versions select different observations", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  res <- lapply(1:3, \(v) {
    rownames(nearmiss(circle_numeric, "class", version = v))
  })
  expect_equal(anyDuplicated(res), 0L)
})

test_that("nearmiss version 2 reaches the under_ratio target", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  result <- nearmiss(circle_numeric, var = "class", version = 2)
  expect_equal(
    unname(table(result$class)[[1]]),
    unname(table(result$class)[[2]])
  )
})

test_that("all distance metrics work for every nearmiss version", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  distances <- c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )
  for (distance in distances) {
    for (version in 1:3) {
      expect_no_error(nearmiss(
        circle_numeric,
        var = "class",
        distance = distance,
        version = version
      ))
    }
  }
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    nearmiss(matrix())
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class")
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example, var = "class", under_ratio = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example[, c("x", "y", "class")], var = "class", version = 0)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(circle_example[, c("x", "y", "class")], var = "class", version = 4)
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(
      circle_example[, c("x", "y", "class")],
      var = "class",
      version = "2"
    )
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(
      circle_example[, c("x", "y", "class")],
      var = "class",
      n_neighbors_ver3 = TRUE
    )
  )
})

test_that("nearmiss version 3 errors when n_neighbors_ver3 is too large", {
  df <- data.frame(
    x = c(0, 10, 1, 2, 3, 4),
    y = rep(0, 6),
    class = factor(c("min", "min", rep("maj", 4)))
  )
  expect_snapshot(
    error = TRUE,
    nearmiss(df, var = "class", k = 1, version = 3, n_neighbors_ver3 = 5)
  )
})
