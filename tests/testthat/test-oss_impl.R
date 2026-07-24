test_that("oss() keeps every minority observation", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 0.3, 100, 101),
    y = rep(0, 6),
    class = factor(c(rep("maj", 4), rep("min", 2)))
  )
  set.seed(1)
  res <- oss(df, var = "class")
  expect_equal(sort(res$x[res$class == "min"]), c(100, 101))
})

test_that("oss_condense() removes redundant majority observations in one pass", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 0.3, 100, 101),
    y = rep(0, 6),
    class = factor(c(rep("maj", 4), rep("min", 2)))
  )
  set.seed(1)
  removed <- oss_condense(df, var = "class")
  expect_equal(removed, c(2, 3, 4))
  expect_equal(sum(df$class[removed] == "min"), 0L)
})

test_that("oss_impl() only removes majority observations", {
  df <- circle_example[, c("x", "y", "class")]
  removed <- withr::with_seed(1, oss_impl(df, var = "class"))
  minority <- names(which.min(table(df$class)))
  expect_equal(sum(as.character(df$class[removed]) == minority), 0L)
})

test_that("oss_impl() removes the union of condensation and majority Tomek links", {
  df <- circle_example[, c("x", "y", "class")]
  removed <- withr::with_seed(1, oss_impl(df, var = "class"))
  condensed <- withr::with_seed(1, oss_condense(df, var = "class"))
  expect_all_true(condensed %in% removed)
})

test_that("oss_condense() returns nothing when there is no majority class", {
  df <- data.frame(
    x = c(0, 1, 2),
    y = rep(0, 3),
    class = factor(rep("a", 3))
  )
  expect_identical(oss_condense(df, var = "class"), integer(0))
})

test_that("distance argument accepted by oss()", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(oss(circle_numeric, var = "class", distance = dist))
  }
})

test_that("bad distance arg errors for oss", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_snapshot(
    error = TRUE,
    oss(circle_numeric, var = "class", distance = "minkowski")
  )
})
