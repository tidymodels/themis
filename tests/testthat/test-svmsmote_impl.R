test_that("svmsmote() balances the classes and keeps the original rows", {
  skip_if_not_installed("kernlab")

  df <- circle_example[, c("x", "y", "class")]
  set.seed(1)
  res <- svmsmote_impl(df, var = "class")

  # Minority class is upsampled to the majority count and the original rows are
  # preserved verbatim as the prefix of the result.
  expect_equal(as.vector(table(res$class)), c(342, 342))
  expect_equal(as.data.frame(res[seq_len(nrow(df)), ]), df)
})

test_that("svmsmote() only adds minority-class rows", {
  skip_if_not_installed("kernlab")

  df <- circle_example[, c("x", "y", "class")]
  set.seed(1)
  res <- svmsmote_impl(df, var = "class")
  synthetic <- tail(res, nrow(res) - nrow(df))
  expect_all_equal(as.character(synthetic$class), "Circle")
})

test_that("svmsmote() is reproducible under a fixed seed", {
  skip_if_not_installed("kernlab")

  df <- circle_example[, c("x", "y", "class")]
  set.seed(1)
  res1 <- svmsmote_impl(df, var = "class")
  set.seed(1)
  res2 <- svmsmote_impl(df, var = "class")
  expect_identical(res1, res2)
})

test_that("svmsmote() errors when the minority class is too small for k", {
  skip_if_not_installed("kernlab")

  df <- data.frame(
    x = c(0, 1, 2, 5, 6, 7, 8, 9, 10, 11),
    y = rep(0, 10),
    class = factor(c(rep("min", 3), rep("maj", 7)))
  )
  expect_snapshot(error = TRUE, svmsmote_impl(df, var = "class", k = 5))
})

test_that("distance argument accepted by svmsmote()", {
  skip_if_not_installed("kernlab")

  circle_numeric <- circle_example[, c("x", "y", "class")]
  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(svmsmote(circle_numeric, var = "class", distance = dist))
  }
})

test_that("bad distance arg errors for svmsmote", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_snapshot(
    error = TRUE,
    svmsmote(circle_numeric, var = "class", distance = "minkowski")
  )
})
