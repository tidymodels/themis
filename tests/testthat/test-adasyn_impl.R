circle_example_num <- circle_example[, c("x", "y", "class")]

test_that("distance argument accepted by adasyn()", {
  expect_no_error(
    adasyn(
      circle_example_num,
      var = "class",
      distance = "euclidean"
    )
  )
  expect_no_error(
    adasyn(
      circle_example_num,
      var = "class",
      distance = "cosine"
    )
  )
  expect_no_error(
    adasyn(
      circle_example_num,
      var = "class",
      distance = "mahalanobis"
    )
  )
  expect_no_error(
    adasyn(
      circle_example_num,
      var = "class",
      distance = "manhattan"
    )
  )
  expect_no_error(
    adasyn(
      circle_example_num,
      var = "class",
      distance = "chebyshev"
    )
  )
})


test_that("bad distance arg errors for adasyn", {
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", distance = "minkowski")
  )
})

test_that("adasyn generates synthetic points only near minority-majority boundary", {
  df <- data.frame(
    x = c(
      52.5,
      57.5,
      62.5,
      67.5, # boundary minority
      0.0,
      0.5,
      1.0,
      1.5, # isolated minority
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65 # majority cluster
    ),
    class = factor(c(rep("min", 8), rep("maj", 16)))
  )
  result <- adasyn(df, var = "class", k = 3, over_ratio = 1)
  synthetic <- tail(result, nrow(result) - nrow(df))
  # Isolated minority (x ∈ [0, 1.5]) has r_value=0 (no majority neighbors)
  # → probability 0 of being selected as a synthetic seed point
  expect_equal(sum(synthetic$x <= 10), 0L)
})

test_that("adasyn weights border points with a single majority neighbor", {
  # Border minority points near a majority point each have exactly one majority
  # neighbor. The `- 1` off-by-one used to zero out their weight (#239), so
  # synthetic points should be seeded from the border cluster, not the isolated
  # minority cluster that has no majority neighbors.
  df <- data.frame(
    x = c(
      10,
      10.1,
      10.2,
      10.3,
      10.4, # border minority cluster
      200,
      200.1,
      200.2,
      200.3, # isolated minority (no majority neighbors)
      10.5, # majority next to the border cluster
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111 # far majority cluster
    ),
    class = factor(c(rep("min", 9), rep("maj", 13)))
  )
  set.seed(1)
  result <- adasyn(df, var = "class", k = 3, over_ratio = 1)
  synthetic <- tail(result, nrow(result) - nrow(df))
  expect_equal(sum(synthetic$x > 30), 0L)
})

test_that("adasyn() interfaces correctly", {
  expect_no_error(adasyn(circle_example_num, var = "class"))

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "Class")
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = c("class", "x"))
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "x")
  )

  circle_example0 <- circle_example_num
  circle_example0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example0, var = "class")
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = 0)
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = -1)
  )

  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = c(5, 10))
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    adasyn(matrix())
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example, var = "class")
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    adasyn(circle_example_num, var = "class", over_ratio = TRUE)
  )
})
