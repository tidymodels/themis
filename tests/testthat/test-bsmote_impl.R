circle_example_num <- circle_example[, c("x", "y", "class")]

test_that("distance argument accepted by bsmote()", {
  expect_no_error(
    bsmote(
      circle_example_num,
      var = "class",
      distance = "euclidean"
    )
  )
  expect_no_error(
    bsmote(
      circle_example_num,
      var = "class",
      distance = "cosine"
    )
  )
  expect_no_error(
    bsmote(
      circle_example_num,
      var = "class",
      distance = "mahalanobis"
    )
  )
  expect_no_error(
    bsmote(
      circle_example_num,
      var = "class",
      distance = "manhattan"
    )
  )
  expect_no_error(
    bsmote(
      circle_example_num,
      var = "class",
      distance = "chebyshev"
    )
  )
})


test_that("bad distance arg errors for bsmote", {
  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", distance = "minkowski")
  )
})

test_that("danger() classifies minority points correctly by neighbor composition", {
  df <- data.frame(
    x = c(0, 0.1, 2.0, 2.1, 2.2, 10),
    class = factor(c("min", "min", "min", "maj", "maj", "maj"))
  )
  k <- 3
  data_mat <- as.matrix(df["x"])
  ids <- RANN::nn2(data_mat, k = k + 1, searchtype = "priority")$nn.idx
  min_class_in <- df$class == "min"
  same_class_count <- rowSums(matrix(min_class_in[ids], ncol = ncol(ids))) - 1
  danger_flags <- themis:::danger(same_class_count, k)
  # Han (2005): a point is in danger when k/2 <= (majority neighbors) < k, i.e.
  # 0 < (minority neighbors) <= k/2. `same_class_count` here is 2,2,1,1,1,0.
  # Row 1 (min@0): 2 minority neighbors, 1 majority → safe (1 < k/2=1.5)
  # Row 2 (min@0.1): 2 minority neighbors, 1 majority → safe
  # Row 3 (min@2.0): 1 minority neighbor, 2 majority → danger (1.5 <= 2 < 3)
  # Rows 4-5 (majority): 1 minority neighbor → danger flag set, but these are
  #   majority rows and are filtered out downstream
  # Row 6 (majority@10): 0 minority neighbors → noise
  expect_equal(danger_flags, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("bsmote all_neighbors=FALSE keeps synthetic points within minority x-range", {
  # Minority at even positions, majority at odd + larger positions (interleaved)
  # → danger minority points exist and generate synthetic samples
  df <- data.frame(
    x = c(0, 2, 4, 6, 8, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19),
    y = rep(0, 15),
    class = factor(c(rep("min", 5), rep("maj", 10)))
  )
  set.seed(42)
  result <- bsmote(
    df,
    var = "class",
    k = 2,
    over_ratio = 1,
    all_neighbors = FALSE
  )
  synthetic <- tail(result, nrow(result) - nrow(df))
  min_x <- min(df$x[df$class == "min"])
  max_x <- max(df$x[df$class == "min"])
  # all_neighbors=FALSE uses only the minority-class matrix for kNN, so
  # synthetic points are convex combinations of minority points → within [0, 8]
  expect_true(all(synthetic$x >= min_x & synthetic$x <= max_x))
})

test_that("bsmote all_neighbors=TRUE can produce synthetic points outside minority bounding box", {
  # Minority: 5 points on the x-axis at x = 0, 2, 4, 6, 8 (bounding box y = 0)
  # Majority: a pair at (x, ±0.5) hugging each minority point
  #
  # With k=3, the 3 nearest non-self neighbors of each minority point in the
  # full matrix are its 2 flanking majority points (dist 0.5) and the closest
  # other minority point (dist 2). That is 2 majority neighbors, so every
  # minority point is "in danger" (k/2 = 1.5 <= 2 < 3). The majority neighbors
  # sit at y = ±0.5, off the y = 0 minority line, so any synthetic interpolated
  # toward one breaches the minority bounding box.
  #
  # With all_neighbors=FALSE the kNN is restricted to the minority-only matrix,
  # so synthetics are convex combinations of the on-axis minority points and
  # stay on y = 0 within x in [0, 8].
  min_x <- c(0, 2, 4, 6, 8)
  df <- data.frame(
    x = c(min_x, rep(min_x, each = 2)),
    y = c(rep(0, 5), rep(c(0.5, -0.5), times = 5)),
    class = factor(c(rep("min", 5), rep("maj", 10)))
  )
  set.seed(42)
  result_true <- bsmote(
    df,
    var = "class",
    k = 3,
    over_ratio = 1,
    all_neighbors = TRUE
  )
  synthetic_true <- tail(result_true, nrow(result_true) - nrow(df))
  expect_true(any(synthetic_true$y > 0 | synthetic_true$y < 0))

  set.seed(42)
  result_false <- bsmote(
    df,
    var = "class",
    k = 3,
    over_ratio = 1,
    all_neighbors = FALSE
  )
  synthetic_false <- tail(result_false, nrow(result_false) - nrow(df))
  expect_true(all(
    synthetic_false$y == 0 &
      synthetic_false$x >= 0 &
      synthetic_false$x <= 8
  ))
})

test_that("bsmote() interfaces correctly", {
  expect_no_error(bsmote(circle_example_num, var = "class"))

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "Class")
  )

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = c("class", "x"))
  )

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "x")
  )

  circle_example0 <- circle_example_num
  circle_example0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example0, var = "class")
  )

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", k = 0)
  )

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", k = -1)
  )

  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", k = c(5, 10))
  )
})

test_that("fractional over_ratio target is rounded (#248)", {
  # round(342 * 0.502) == 172, truncation would give 171
  result <- bsmote(circle_example_num, var = "class", over_ratio = 0.502)
  expect_equal(sum(result$class == "Circle"), 172)
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    bsmote(matrix())
  )
  expect_snapshot(
    error = TRUE,
    bsmote(circle_example, var = "class")
  )
  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", over_ratio = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    bsmote(circle_example_num, var = "class", all_neighbors = 1)
  )
})
