circle_example_num <- circle_example[, c("x", "y", "class")]

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
  # Row 1 (min@0): 2 same-class neighbors → danger (k/2=1.5 ≤ 2 < 3)
  # Row 2 (min@0.1): 2 same-class neighbors → danger
  # Row 3 (min@2.0): 1 same-class neighbor → noise (1 < k/2=1.5)
  # Rows 4-6 (majority): not danger
  expect_equal(danger_flags, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
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
  # Minority: 4 points in a diamond at radius 1 — bounding box [-1,1] x [-1,1]
  # Majority: 36 points, all strictly outside the minority bounding box
  #
  # With k=3, the 3 nearest non-self neighbors of each minority point in the
  # full matrix are: the 2 adjacent minority points (dist √2 ≈ 1.41) and the
  # nearest majority point at radius 2.5 (dist 1.5). The majority neighbor lies
  # outside [-1,1] x [-1,1], so any synthetic interpolated toward it breaches
  # the minority bounding box.
  #
  # With all_neighbors=FALSE the kNN is restricted to the minority-only matrix,
  # so no majority point can ever be an interpolation target, and synthetics
  # are guaranteed to stay within the minority convex hull.
  #
  # over_ratio=1 → 32 synthetic minority samples (≈8 per danger point), making
  # it near-certain that the 3rd neighbor (majority) is drawn at least once.
  maj <- expand.grid(
    x = c(-3, -2.5, -1.5, 1.5, 2.5, 3),
    y = c(-3, -2, 0, 1.5, 2, 3)
  )
  df <- data.frame(
    x = c(1, 0, -1, 0, maj$x),
    y = c(0, 1, 0, -1, maj$y),
    class = factor(c(rep("min", 4), rep("maj", nrow(maj))))
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
  expect_true(any(
    synthetic_true$x < -1 |
      synthetic_true$x > 1 |
      synthetic_true$y < -1 |
      synthetic_true$y > 1
  ))

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
    synthetic_false$x >= -1 &
      synthetic_false$x <= 1 &
      synthetic_false$y >= -1 &
      synthetic_false$y <= 1
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
