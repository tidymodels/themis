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
})
