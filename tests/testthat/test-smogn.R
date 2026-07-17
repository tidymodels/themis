test_that("distance argument accepted by step_smogn()", {
  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(
      recipe(y ~ x, data = circle_example) |>
        step_smogn(y, distance = dist) |>
        prep() |>
        bake(new_data = NULL)
    )
  }
})

test_that("bad distance arg for step_smogn()", {
  expect_snapshot(
    error = TRUE,
    recipe(y ~ x, data = circle_example) |>
      step_smogn(y, distance = "L2") |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("rare regions are over-sampled and common regions under-sampled", {
  set.seed(1)
  df <- data.frame(x = rnorm(200), y = c(rnorm(190), 8 + rnorm(10)))

  res <- recipe(y ~ x, data = df) |>
    step_smogn(y, seed = 1) |>
    prep() |>
    bake(new_data = NULL)

  n_rare_orig <- sum(df$y > 4)
  n_rare_new <- sum(res$y > 4)
  n_common_orig <- sum(df$y <= 4)
  n_common_new <- sum(res$y <= 4)

  expect_gt(n_rare_new, n_rare_orig)
  expect_lt(n_common_new, n_common_orig)
})

test_that("custom relevance control points are respected", {
  rel <- matrix(
    c(
      min(circle_example$y),
      stats::median(circle_example$y),
      max(circle_example$y),
      1,
      0,
      1
    ),
    ncol = 2
  )

  expect_no_error(
    recipe(y ~ x, data = circle_example) |>
      step_smogn(y, relevance = rel, seed = 1) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(y ~ x, data = circle_example) |>
      step_smogn(y, seed = seed) |>
      prep() |>
      bake(new_data = NULL) |>
      pull(y)
  }

  run_1 <- step_with_seed(seed = 1234)
  run_2 <- step_with_seed(seed = 1234)
  run_3 <- step_with_seed(seed = 12345)

  expect_equal(run_1, run_2)
  expect_false(identical(run_1, run_3))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric outcome check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_smogn(class) |>
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_smogn(x, y) |>
      prep()
  )
})

test_that("errors if predictors are not numeric", {
  expect_snapshot(
    error = TRUE,
    recipe(y ~ x + class, data = circle_example) |>
      step_smogn(y) |>
      prep()
  )
})

test_that("NA in data", {
  df <- circle_example[, c("x", "y")]
  df$x[1] <- NA

  expect_snapshot(
    error = TRUE,
    recipe(y ~ x, data = df) |>
      step_smogn(y) |>
      prep()
  )
})

test_that("test tidy()", {
  rec <- recipe(y ~ x, data = circle_example) |>
    step_smogn(y, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(terms = "y", id = "")
  trained <- tibble(terms = "y", id = "")

  expect_equal(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("ordering of kept points are preserved", {
  res <- recipe(y ~ x + id, data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_smogn(y, indicator_column = ".syn", seed = 1) |>
    prep() |>
    bake(new_data = NULL)

  # synthetic rows have NA in the ignored column, kept rows do not
  expect_equal(sum(is.na(res$id)), sum(res$.syn))
})

test_that("non-predictor variables are ignored", {
  res <- recipe(y ~ x + id, data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_smogn(y, seed = 1) |>
    prep() |>
    bake(new_data = NULL)

  expect_true("id" %in% names(res))
})

test_that("tunable", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_smogn(mpg)
  rec_param <- tunable.step_smogn(rec$steps[[1]])
  expect_equal(rec_param$name, c("neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("indicator_column adds logical column marking synthetic rows", {
  rec <- recipe(y ~ x, data = circle_example) |>
    step_smogn(y, indicator_column = ".new_row") |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_true(".new_row" %in% names(res))
  expect_type(res$.new_row, "logical")
  expect_gt(sum(res$.new_row), 0L)
})

test_that("indicator_column bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(y ~ x, data = circle_example) |>
      step_smogn(y, indicator_column = 1)
  )
  expect_snapshot(
    error = TRUE,
    recipe(y ~ x, data = circle_example) |>
      step_smogn(y, indicator_column = "x") |>
      prep()
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_smogn(mpg, threshold = 2) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_smogn(mpg, neighbors = TRUE) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_smogn(mpg, perturbation = -1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_smogn(mpg, seed = TRUE)
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_smogn(mpg, neighbors = hardhat::tune())

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(y ~ x, data = circle_example) |>
    step_smogn(y, skip = FALSE) |>
    add_role(y, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = circle_example, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = circle_example[, -2])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_smogn(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_smogn(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_smogn(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(y ~ x, data = circle_example) |>
    step_smogn(y)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(y ~ x, data = circle_example) |>
    step_smogn(y, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 1))), 1L)
})
