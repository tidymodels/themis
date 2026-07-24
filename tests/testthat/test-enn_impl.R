test_that("enn() removes an observation disagreeing with its neighbors", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 10, 10.1, 10.2, 0.15),
    y = rep(0, 7),
    class = factor(c("a", "a", "a", "b", "b", "b", "b"))
  )
  # Point 7 (x = 0.15, class b) sits amongst class-a points, so its 3 nearest
  # neighbors are all "a" and it is the only removal.
  expect_equal(enn_impl(df, var = "class", neighbors = 3), 7L)
  expect_equal(nrow(enn(df, var = "class", neighbors = 3)), 6L)
})

test_that("enn() removes nothing when classes are cleanly separated", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 0.3, 10, 10.1, 10.2, 10.3),
    y = rep(0, 8),
    class = factor(c(rep("a", 4), rep("b", 4)))
  )
  expect_identical(enn_impl(df, var = "class", neighbors = 3), integer(0))
})

test_that("all_k removes at least as much as a single pass", {
  df <- data.frame(
    x = c(0, 0.1, 0.2, 10, 10.1, 10.2, 0.15, 9.9),
    y = rep(0, 8),
    class = factor(c("a", "a", "a", "b", "b", "b", "b", "a"))
  )
  single <- enn_impl(df, var = "class", neighbors = 3)
  allk <- enn_impl(df, var = "class", neighbors = 3, all_k = TRUE)
  expect_all_true(single %in% allk)
})

test_that("enn_impl() errors when too few observations for neighbors", {
  df <- data.frame(
    x = c(0, 1, 2),
    y = rep(0, 3),
    class = factor(c("a", "a", "b"))
  )
  expect_snapshot(error = TRUE, enn_impl(df, var = "class", neighbors = 5))
})
