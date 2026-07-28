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

test_that("nn_dists_cross() returns cosine-distance magnitudes 1 - cos_sim (#244)", {
  query <- matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)
  reference <- matrix(c(0, 1, 2, 0, 3, 3), ncol = 2, byrow = TRUE)

  cos_sim <- (query / sqrt(rowSums(query^2))) %*%
    t(reference / sqrt(rowSums(reference^2)))
  expected <- t(apply(1 - cos_sim, 1, sort))

  d <- nn_dists_cross(query, reference, k = 3, distance = "cosine")
  expect_equal(d, expected)
})

# Rows on the probability simplex, chosen so no two pairwise distances tie.
simplex_fixture <- function() {
  rbind(
    c(0.60, 0.30, 0.10),
    c(0.50, 0.42, 0.08),
    c(0.10, 0.21, 0.69),
    c(0.22, 0.18, 0.60),
    c(0.34, 0.33, 0.33)
  )
}

# Direct formulas for the sqrt-embedded metrics, used to check that the RANN
# search on square-rooted coordinates agrees with the definitions.
divergence_matrix <- function(x, y, distance) {
  f <- switch(
    distance,
    "squared_chord" = \(a, b) sum((sqrt(a) - sqrt(b))^2),
    "matusita" = \(a, b) sqrt(sum((sqrt(a) - sqrt(b))^2)),
    "hellinger" = \(a, b) 2 * sqrt(1 - sum(sqrt(a * b))),
    "bhattacharyya" = \(a, b) -log(sum(sqrt(a * b)))
  )
  outer(
    seq_len(nrow(x)),
    seq_len(nrow(y)),
    Vectorize(function(i, j) f(x[i, ], y[j, ]))
  )
}

test_that("nn_indices() matches brute-force sqrt-embedded divergence neighbors", {
  data <- simplex_fixture()
  expect_nn <- function(distance) {
    d <- divergence_matrix(data, data, distance)
    expected <- t(apply(d, 1, \(x) order(x)[seq_len(3)]))
    expect_equal(nn_indices(data, k = 2, distance), expected)
  }

  expect_nn("squared_chord")
  expect_nn("matusita")
  expect_nn("hellinger")
  expect_nn("bhattacharyya")
})

test_that("nn_indices_cross() matches brute-force sqrt-embedded neighbors", {
  data <- simplex_fixture()
  query <- data[1:2, ]
  reference <- data[3:5, ]
  expect_nn <- function(distance) {
    d <- divergence_matrix(query, reference, distance)
    expected <- t(apply(d, 1, \(x) order(x)[seq_len(2)]))
    expect_equal(nn_indices_cross(query, reference, k = 2, distance), expected)
  }

  expect_nn("squared_chord")
  expect_nn("matusita")
  expect_nn("hellinger")
  expect_nn("bhattacharyya")
})

test_that("nn_dists_cross() returns true divergence magnitudes, not euclidean", {
  data <- simplex_fixture()
  query <- data[1:2, ]
  reference <- data[3:5, ]
  expect_dists <- function(distance) {
    d <- divergence_matrix(query, reference, distance)
    expected <- t(apply(d, 1, \(x) sort(x)[seq_len(2)]))
    expect_equal(nn_dists_cross(query, reference, k = 2, distance), expected)
  }

  expect_dists("squared_chord")
  expect_dists("matusita")
  expect_dists("hellinger")
  expect_dists("bhattacharyya")
})

test_that("bhattacharyya distance is infinite for disjoint support", {
  query <- matrix(c(1, 0, 0, 0), ncol = 2, byrow = TRUE)
  reference <- matrix(c(0, 1), ncol = 2)
  expect_equal(
    nn_dists_cross(query[1, , drop = FALSE], reference, 1, "bhattacharyya"),
    matrix(Inf)
  )
})

test_that("sqrt-embedded metrics reject non-distribution predictors", {
  negative <- rbind(c(0.5, -0.5, 1), c(0.2, 0.3, 0.5))
  expect_snapshot(error = TRUE, nn_indices(negative, 1, "matusita"))

  unnormalized <- rbind(c(1, 2, 3), c(3, 2, 1), c(1, 1, 4))
  expect_snapshot(error = TRUE, nn_indices(unnormalized, 1, "hellinger"))
  expect_snapshot(error = TRUE, nn_indices(unnormalized, 1, "bhattacharyya"))

  # squared_chord and matusita hold off the simplex, so they are allowed
  expect_no_error(nn_indices(unnormalized, 1, "squared_chord"))
  expect_no_error(nn_indices(unnormalized, 1, "matusita"))
})

test_that("mahalanobis whitening reproduces stats::mahalanobis distances (#237)", {
  set.seed(1)
  n <- 200
  x1 <- rnorm(n)
  data <- cbind(x1, 2 * x1 + rnorm(n, sd = 0.3), rnorm(n), x1 - rnorm(n))
  S <- stats::cov(data)

  whitened <- data %*% solve(chol(S))
  center <- whitened[1, ]
  themis_d2 <- rowSums(sweep(whitened, 2, center)^2)
  true_d2 <- stats::mahalanobis(data, data[1, ], S)

  expect_equal(themis_d2, unname(true_d2))
})

test_that("drop_self_neighbor() removes self by row index for duplicates (#247)", {
  # Self in the first column (typical, non-duplicate case)
  idx <- rbind(c(1L, 2L, 3L), c(2L, 1L, 3L), c(3L, 1L, 2L))
  expect_identical(
    drop_self_neighbor(idx),
    rbind(c(2L, 3L), c(1L, 3L), c(1L, 2L))
  )

  # Self not in the first column (duplicate coordinates)
  idx <- rbind(c(2L, 1L, 3L), c(1L, 2L, 3L))
  expect_identical(drop_self_neighbor(idx), rbind(c(2L, 3L), c(1L, 3L)))

  # Self missing entirely: drop the farthest (last) neighbor
  idx <- rbind(c(2L, 3L, 4L), c(3L, 4L, 1L))
  expect_identical(drop_self_neighbor(idx), rbind(c(2L, 3L), c(3L, 4L)))
})
