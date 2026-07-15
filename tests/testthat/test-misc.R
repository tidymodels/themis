test_that("nn_indices() errors informatively for singular mahalanobis covariance", {
  # More predictors than observations -> singular covariance matrix
  data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  expect_snapshot(error = TRUE, nn_indices(data, 1, "mahalanobis"))
})

test_that("nn_indices() uses the correct distance metric", {
  # Each case is constructed so the nearest neighbor differs by metric.
  # Row 1 is the query point; we check which of rows 2/3 it picks as neighbor.
  nn1 <- function(ids, row) unname(ids[row, ids[row, ] != row])[[1]]

  # Manhattan vs euclidean:
  # From (0,0): manhattan to (1,0) = 1 < manhattan to (0.6,0.6) = 1.2
  #             euclidean to (1,0) = 1 > euclidean to (0.6,0.6) = 0.849
  data_manh <- matrix(c(0, 0, 1, 0, 0.6, 0.6), ncol = 2, byrow = TRUE)
  expect_equal(nn1(nn_indices(data_manh, 1, "euclidean"), 1), 3L)
  expect_equal(nn1(nn_indices(data_manh, 1, "manhattan"), 1), 2L)

  # Chebyshev vs euclidean:
  # From (0,0): chebyshev to (0.7,0.7) = 0.7 < chebyshev to (0.9,0) = 0.9
  #             euclidean to (0.7,0.7) = 0.99 > euclidean to (0.9,0) = 0.9
  data_cheb <- matrix(c(0, 0, 0.9, 0, 0.7, 0.7), ncol = 2, byrow = TRUE)
  expect_equal(nn1(nn_indices(data_cheb, 1, "euclidean"), 1), 2L)
  expect_equal(nn1(nn_indices(data_cheb, 1, "chebyshev"), 1), 3L)

  # Cosine vs euclidean:
  # (1,1) and (100,100) point in the same direction: cosine distance = 0
  # euclidean distance to (100,100) >> distance to (1.1,0)
  data_cos <- matrix(c(1, 1, 100, 100, 1.1, 0), ncol = 2, byrow = TRUE)
  expect_equal(nn1(nn_indices(data_cos, 1, "euclidean"), 1), 3L)
  expect_equal(nn1(nn_indices(data_cos, 1, "cosine"), 1), 2L)

  # Mahalanobis vs euclidean:
  # Cloud points span ±0.1 in x and ±20 in y, making y-variance ~200x larger.
  # From (0,0): euclidean to (0.5,0) = 0.5 < euclidean to (0,5) = 5
  #             mahalanobis to (0,5) < mahalanobis to (0.5,0) since a step of 5
  #             in y is small relative to the y-variance
  data_mahal <- matrix(
    c(0, 0, 0.5, 0, 0, 5, -0.1, -20, -0.1, 20, 0.1, -20, 0.1, 20),
    ncol = 2,
    byrow = TRUE
  )
  expect_equal(nn1(nn_indices(data_mahal, 1, "euclidean"), 1), 2L)
  expect_equal(nn1(nn_indices(data_mahal, 1, "mahalanobis"), 1), 3L)
})
