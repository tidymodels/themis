circle_numeric <- circle_example[, c("x", "y", "class")]

test_that("basic usage", {
  res <- cluster_centroids(circle_numeric, var = "class")

  expect_named(res, names(circle_numeric))
  expect_all_equal(
    as.vector(table(res$class)),
    min(table(
      circle_numeric$class
    ))
  )
})

test_that("under_ratio works", {
  res <- cluster_centroids(circle_numeric, var = "class", under_ratio = 1.5)

  expect_equal(
    sort(as.numeric(table(res$class))),
    min(table(circle_numeric$class)) * c(1, 1.5)
  )
})

test_that("classes at or below the target are untouched", {
  res <- cluster_centroids(circle_numeric, var = "class", under_ratio = 100)

  expect_equal(res, cluster_centroids(circle_numeric, "class", under_ratio = 6))
  expect_equal(table(res$class), table(circle_numeric$class))
})

test_that("soft voting generates new observations", {
  res <- cluster_centroids(circle_numeric, var = "class", voting = "soft")

  minority <- res[res$class == "Circle", ]
  centroids <- res[res$class == "Rest", ]

  # untouched class is passed through, generated class contains new points
  # (a singleton cluster can still have an input row as its centroid)
  expect_equal(
    minority,
    circle_numeric[circle_numeric$class == "Circle", ],
    ignore_attr = "row.names"
  )
  expect_gt(nrow(dplyr::setdiff(centroids, circle_numeric)), 0)
})

test_that("hard voting only keeps rows of the under-sampled class (#318)", {
  res <- cluster_centroids(circle_numeric, var = "class", voting = "hard")

  expect_equal(nrow(dplyr::setdiff(res, circle_numeric)), 0L)

  kept <- dplyr::semi_join(
    circle_numeric[circle_numeric$class == "Rest", ],
    res,
    by = c("x", "y")
  )
  expect_all_equal(as.character(kept$class), "Rest")
})

test_that("factor levels are kept", {
  res <- cluster_centroids(circle_numeric, var = "class")

  expect_equal(levels(res$class), levels(circle_numeric$class))
})

test_that("works with a character `var`", {
  df <- circle_numeric
  df$class <- as.character(df$class)

  res <- cluster_centroids(df, "class")

  expect_identical(sort(unique(res$class)), c("Circle", "Rest"))
  expect_identical(sum(is.na(res$class)), 0L)
})

test_that("errors when there are too few distinct observations", {
  df <- data.frame(
    x = c(rep(1, 20), 1:3),
    y = c(rep(1, 20), 1:3),
    class = factor(c(rep("majority", 20), rep("minority", 3)))
  )

  expect_snapshot(error = TRUE, cluster_centroids(df, "class"))
})

test_that("bad args", {
  expect_snapshot(error = TRUE, cluster_centroids(circle_numeric, "x"))
  expect_snapshot(
    error = TRUE,
    cluster_centroids(circle_example, "class")
  )
  expect_snapshot(
    error = TRUE,
    cluster_centroids(circle_numeric, "class", under_ratio = "yes")
  )
  expect_snapshot(
    error = TRUE,
    cluster_centroids(circle_numeric, "class", voting = "medium")
  )
})
