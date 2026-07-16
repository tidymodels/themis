test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_all_true(as.vector(te_xtab <= og_xtab))

  expect_no_warning(prep(rec1))
})

test_that("only majority classes are removed", {
  res <- ncl(
    circle_example[, c("x", "y", "class")],
    var = "class"
  )

  counts <- table(res$class)
  orig <- table(circle_example$class)

  minority <- names(orig)[which.min(orig)]

  expect_identical(counts[[minority]], orig[[minority]])
  expect_lt(counts[["Rest"]], orig[["Rest"]])
})

test_that("neighbors argument changes result", {
  baked1 <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class, neighbors = 1) |>
    prep() |>
    bake(new_data = NULL)

  baked5 <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class, neighbors = 5) |>
    prep() |>
    bake(new_data = NULL)

  expect_false(identical(nrow(baked1), nrow(baked5)))
})

test_that("threshold_clean argument changes result", {
  circle_numeric <- circle_example[, c("x", "y", "class")]

  n_low <- nrow(ncl(
    circle_numeric,
    var = "class",
    threshold_clean = 0
  ))
  n_high <- nrow(ncl(
    circle_numeric,
    var = "class",
    threshold_clean = 100
  ))

  expect_lte(n_low, n_high)
})

test_that("errors when not enough observations for neighbors", {
  small <- circle_example[1:3, c("x", "y", "class")]

  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = small) |>
      step_ncl(class, neighbors = 5, skip = FALSE) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("works with a single predictor", {
  skip_if_not_installed("modeldata")

  data("hpc_data", package = "modeldata")

  expect_no_error(
    recipe(class ~ compounds, data = hpc_data) |>
      step_ncl(all_outcomes()) |>
      prep() |>
      bake(NULL)
  )
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_ncl(x) |>
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_ncl(class, id) |>
      prep()
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = df_char) |>
      step_ncl(x) |>
      prep()
  )
})

test_that("NA in response", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")
  credit_data0 <- credit_data
  credit_data0[1, 1] <- NA

  expect_snapshot(
    error = TRUE,
    recipe(Status ~ Age, data = credit_data0) |>
      step_ncl(Status) |>
      prep()
  )
})

test_that("test tidy()", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(
    terms = "class",
    id = ""
  )

  trained <- tibble(
    terms = "class",
    id = ""
  )

  expect_equal(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
  circle_example_alt_levels <- list()
  for (i in 1:4) {
    circle_example_alt_levels[[i]] <- circle_example
  }

  # Checking for forgetting levels by majority/minor switching
  for (i in c(2, 4)) {
    levels(circle_example_alt_levels[[i]]$class) <-
      rev(levels(circle_example_alt_levels[[i]]$class))
  }

  # Checking for forgetting levels by alphabetical switching
  for (i in c(3, 4)) {
    circle_example_alt_levels[[i]]$class <-
      factor(
        x = circle_example_alt_levels[[i]]$class,
        levels = rev(levels(circle_example_alt_levels[[i]]$class))
      )
  }

  for (i in 1:4) {
    rec_p <- recipe(class ~ x + y, data = circle_example_alt_levels[[i]]) |>
      step_ncl(class) |>
      prep()

    expect_equal(
      levels(circle_example_alt_levels[[i]]$class), # Original levels
      rec_p$levels$class$values # New levels
    )
    expect_equal(
      levels(circle_example_alt_levels[[i]]$class), # Original levels
      levels(bake(rec_p, new_data = NULL)$class) # New levels
    )
  }
})

test_that("distance_with allows non-numeric columns to be present", {
  df_mixed <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(20, 0.5, 1)),
    y = c(rnorm(50, 0, 1), rnorm(20, 0.5, 1)),
    name = c(rep("alice", 50), rep("bob", 20)),
    class = factor(c(rep("majority", 50), rep("minority", 20)))
  )

  expect_no_error(
    recipe(class ~ ., data = df_mixed) |>
      step_ncl(class, distance_with = c(x, y)) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("distance_with errors on non-numeric column", {
  df_mixed <- data.frame(
    x = c(1:5, 1:2),
    name = c(rep("a", 5), rep("b", 2)),
    class = factor(c(rep("majority", 5), rep("minority", 2)))
  )

  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = df_mixed) |>
      step_ncl(class, distance_with = c(x, name)) |>
      prep()
  )
})

test_that("id variables are ignored", {
  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_ncl(class) |>
    prep()

  expect_equal(ncol(bake(rec_id, new_data = NULL)), 4)
})

test_that("id variables don't turn predictors to factors", {
  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_ncl(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})

test_that("distance argument accepted by step_ncl()", {
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, distance = "euclidean") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, distance = "cosine") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, distance = "mahalanobis") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, distance = "manhattan") |>
      prep() |>
      bake(new_data = NULL)
  )
  expect_no_error(
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, distance = "chebyshev") |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("bad distance arg for step_ncl()", {
  expect_snapshot(
    error = TRUE,
    bake(
      prep(step_ncl(
        recipe(class ~ x + y, data = circle_example),
        class,
        distance = "L2"
      )),
      new_data = NULL
    )
  )
})

test_that("tunable", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class)

  tune_args <- tunable(rec$steps[[1]])
  expect_equal(tune_args$name, "neighbors")
  expect_equal(nrow(tune_args), 1L)
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_ncl(seed = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, neighbors = -1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_ncl(class, threshold_clean = -1) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class, skip = FALSE) |>
    add_role(class, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = circle_example, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = circle_example[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ncl(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_ncl(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_ncl(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_ncl(class, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 1))), 1L)
})
