test_that("distance argument accepted by tomek()", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_no_error(tomek(circle_numeric, var = "class", distance = "euclidean"))
  expect_no_error(tomek(circle_numeric, var = "class", distance = "cosine"))
  expect_no_error(tomek(
    circle_numeric,
    var = "class",
    distance = "mahalanobis"
  ))
  expect_no_error(tomek(circle_numeric, var = "class", distance = "manhattan"))
  expect_no_error(tomek(circle_numeric, var = "class", distance = "chebyshev"))
})

test_that("bad distance arg errors for tomek", {
  circle_numeric <- circle_example[, c("x", "y", "class")]
  expect_snapshot(
    error = TRUE,
    tomek(circle_numeric, var = "class", distance = "minkowski")
  )
})

test_that("tomek removes exactly the points forming Tomek links", {
  df <- data.frame(
    x = c(0, 0.9, 3, 10, 11),
    class = factor(c("min", "maj", "min", "maj", "maj"))
  )
  result <- tomek(df, var = "class")
  # Pair {1, 2}: mutual NN, different class → Tomek link, both removed
  # Point 3: NN is point 2, but point 2's NN is point 1 → not a link
  # Pair {4, 5}: mutual NN, same class → not a link
  expect_equal(result$x, c(3, 10, 11))
  expect_equal(as.character(result$class), c("min", "maj", "maj"))
})

test_that("order doesn't matter", {
  df <- data.frame(
    target = rep(c("Yes", "No"), c(10, 50)),
    x = rep(c(1, 2, 3), c(9, 2, 49))
  )
  expect_equal(c(10, 11), themis:::tomek_impl(df, "target"))

  df <- data.frame(
    target = rep(c("Yes", "No"), c(50, 10)),
    x = rep(c(1, 2, 3), c(49, 2, 9))
  )
  expect_equal(c(50, 51), themis:::tomek_impl(df, "target"))
})


test_that("tomek() interfaces correctly", {
  circle_example_num <- circle_example[, 1:3]

  expect_no_error(tomek(circle_example_num, var = "class"))

  expect_snapshot(
    error = TRUE,
    tomek(circle_example_num, var = "Class")
  )

  expect_snapshot(
    error = TRUE,
    tomek(circle_example_num, var = c("class", "x"))
  )

  expect_snapshot(
    error = TRUE,
    tomek(circle_example_num, var = "x")
  )

  circle_example0 <- circle_example_num
  circle_example0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    tomek(circle_example0, var = "class")
  )
})

test_that("ordering of columns shouldn't matter", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")

  credit_data0 <- credit_data |>
    filter(!is.na(Job)) |>
    select(Job, Time, Age, Expenses)

  expect_no_error(
    tomek(credit_data0, "Job")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    bsmote(matrix())
  )
  expect_snapshot(
    error = TRUE,
    tomek(circle_example, var = "class")
  )
})
