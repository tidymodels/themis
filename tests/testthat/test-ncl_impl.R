test_that("ncl() removes misclassified majority observations (A1)", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 10, 10.1, 10.2, 0.15),
    y = rep(0, 7),
    class = factor(c("a", "a", "a", "b", "b", "b", "b"))
  )
  # Class b has 4 members (majority); the stray b at x = 0.15 is misclassified
  # by its a-neighbors and removed by the ENN step.
  expect_equal(ncl_impl(df, var = "class", neighbors = 3), 7L)
})

test_that("ncl() cleans majority neighbors of misclassified minority points (A2)", {
  df <- data.frame(
    x = c(0, 10, 11, 12, 20, 21, 22),
    y = rep(0, 7),
    class = factor(c("min", rep("maj", 6)))
  )
  # The lone minority point at x = 0 is surrounded by majority points, so its 3
  # nearest majority neighbors (x = 10, 11, 12) are removed.
  expect_equal(ncl_impl(df, var = "class", neighbors = 3), c(2, 3, 4))
})

test_that("ncl() removes nothing when classes are cleanly separated", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 0.3, 10, 10.1, 10.2, 10.3),
    y = rep(0, 8),
    class = factor(c(rep("a", 4), rep("b", 4)))
  )
  expect_identical(ncl_impl(df, var = "class", neighbors = 3), integer(0))
})

test_that("ncl_impl() errors when too few observations for neighbors", {
  df <- data.frame(
    x = c(0, 1, 2),
    y = rep(0, 3),
    class = factor(c("a", "a", "b"))
  )
  expect_snapshot(error = TRUE, ncl_impl(df, var = "class", neighbors = 5))
})
