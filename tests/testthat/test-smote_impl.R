test_that("distance argument accepted by smote()", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_no_error(
    smote(
      circle_numeric,
      var = "class",
      distance = "euclidean"
    )
  )
  expect_no_error(
    smote(
      circle_numeric,
      var = "class",
      distance = "cosine"
    )
  )
  expect_no_error(
    smote(
      circle_numeric,
      var = "class",
      distance = "mahalanobis"
    )
  )
  expect_no_error(
    smote(
      circle_numeric,
      var = "class",
      distance = "manhattan"
    )
  )
  expect_no_error(
    smote(
      circle_numeric,
      var = "class",
      distance = "chebyshev"
    )
  )
})


test_that("distance variants produce valid synthetic data", {
  circle_numeric <- circle_example[, c("x", "y", "class")]

  result <- smote(circle_numeric, var = "class", distance = "euclidean")
  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(names(circle_numeric)))
  expect_gt(nrow(result), nrow(circle_numeric))

  result <- smote(circle_numeric, var = "class", distance = "cosine")
  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(names(circle_numeric)))
  expect_gt(nrow(result), nrow(circle_numeric))

  result <- smote(circle_numeric, var = "class", distance = "mahalanobis")
  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(names(circle_numeric)))
  expect_gt(nrow(result), nrow(circle_numeric))

  result <- smote(circle_numeric, var = "class", distance = "manhattan")
  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(names(circle_numeric)))
  expect_gt(nrow(result), nrow(circle_numeric))

  result <- smote(circle_numeric, var = "class", distance = "chebyshev")
  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), sort(names(circle_numeric)))
  expect_gt(nrow(result), nrow(circle_numeric))
})

test_that("bad distance arg errors", {
  expect_snapshot(
    error = TRUE,
    smote(
      circle_example[, c("x", "y", "class")],
      var = "class",
      distance = "minkowski"
    )
  )
})

test_that("smote synthetic points lie within the minority class x-range", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, 100, 101, 102, 103, 104, 105),
    class = factor(c(rep("min", 5), rep("maj", 6)))
  )
  result <- smote(df, var = "class", k = 3, over_ratio = 1)
  synthetic <- tail(result, nrow(result) - nrow(df))
  # Each synthetic point is: minority_point + t * (neighbor - minority_point), t ∈ [0,1]
  # Both endpoints are minority points in [1, 5], so synthetics must stay in [1, 5]
  expect_true(all(synthetic$x >= 1 & synthetic$x <= 5))
})

test_that("samples stay inside convex hull of data.", {
  rdata <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2)

  expect_true(all(dplyr::between(smote_data(rdata, 3, 100), 0, 1)))
})

test_that("order doesn't matter", {
  df <- data.frame(
    target = rep(c("Yes", "No"), c(10, 50)),
    x = rep(1:2, c(10, 50))
  )
  expect_equal(100, nrow(themis:::smote_impl(df, "target", 5, 1)))

  df <- data.frame(
    target = rep(c("Yes", "No"), c(50, 10)),
    x = rep(1:2, c(50, 10))
  )
  expect_equal(100, nrow(themis:::smote_impl(df, "target", 5, 1)))
})


test_that("smote() interfaces correctly", {
  circle_example_num <- circle_example[, 1:3]

  expect_no_error(smote(circle_example_num, var = "class"))

  expect_snapshot(
    error = TRUE,
    smote(circle_example_num, var = "Class")
  )

  expect_snapshot(
    error = TRUE,
    smote(circle_example_num, var = c("class", "x"))
  )

  expect_snapshot(
    error = TRUE,
    smote(circle_example_num, var = "x")
  )

  circle_example0 <- circle_example_num
  circle_example0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    smote(circle_example0, var = "class")
  )

  expect_snapshot(
    error = TRUE,
    smote(circle_example_num, var = "class", k = 0)
  )

  expect_snapshot(
    error = TRUE,
    smote(circle_example_num, var = "class", k = -1)
  )

  expect_snapshot(
    error = TRUE,
    smote(circle_example_num, var = "class", k = c(5, 10))
  )
})

test_that("ordering of columns shouldn't matter", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")

  credit_data0 <- credit_data |>
    filter(!is.na(Job)) |>
    select(Job, Time, Age, Expenses)

  expect_no_error(
    smote(credit_data0, "Job", over_ratio = 1)
  )
})

test_that("Doesn't error if no upsampling is done (#119)", {
  dat <- data.frame(
    outcome = c(rep("X", 101), rep("Z", 50)),
    X1 = 1
  )

  expect_no_error(
    smote_impl(dat, "outcome", 5, over_ratio = 0.5)
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    smote(matrix())
  )
  expect_snapshot(
    error = TRUE,
    smote(circle_example, var = "class")
  )
  expect_snapshot(
    error = TRUE,
    smote(
      data.frame(
        x = 1:4,
        a = letters[1:4],
        b = letters[1:4],
        class = factor(1:4)
      ),
      var = "class"
    )
  )
  expect_snapshot(
    error = TRUE,
    smote(circle_example, var = "class", k = 0)
  )
  expect_snapshot(
    error = TRUE,
    smote(circle_example, var = "class", k = 5.5)
  )
  expect_snapshot(
    error = TRUE,
    smote(circle_example, var = "class", over_ratio = TRUE)
  )
})
