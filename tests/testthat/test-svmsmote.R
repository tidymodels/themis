test_that("distance argument accepted by step_svmsmote()", {
  skip_if_not_installed("kernlab")

  for (dist in c(
    "euclidean",
    "cosine",
    "mahalanobis",
    "manhattan",
    "chebyshev"
  )) {
    expect_no_error(
      recipe(class ~ x + y, data = circle_example) |>
        step_svmsmote(class, distance = dist) |>
        prep() |>
        bake(new_data = NULL)
    )
  }
})

test_that("bad distance arg for step_svmsmote()", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_svmsmote(class, distance = "L2") |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("errors if there isn't enough data", {
  skip_if_not_installed("kernlab")
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")
  credit_data0 <- credit_data

  credit_data0$Status <- as.character(credit_data0$Status)
  credit_data0$Status[1] <- "dummy"
  credit_data0$Status <- as.factor(credit_data0$Status)

  expect_snapshot(
    error = TRUE,
    recipe(Status ~ Age, data = credit_data0) |>
      step_svmsmote(Status) |>
      prep()
  )
})

test_that("all minority classes are upsampled", {
  skip_if_not_installed("kernlab")
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")
  rec1_p2 <- recipe(
    species ~ bill_length_mm + bill_depth_mm,
    data = penguins
  ) |>
    step_impute_mean(all_predictors()) |>
    step_svmsmote(species) |>
    prep() |>
    bake(new_data = NULL)

  expect_true(all(max(table(rec1_p2$species)) == 152))
})

test_that("basic usage", {
  skip_if_not_installed("kernlab")

  rec1 <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))
  expect_no_warning(prep(rec1))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_svmsmote(x) |>
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_svmsmote(class, id) |>
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
      step_svmsmote(x) |>
      prep()
  )
})

test_that("NA in response", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")

  expect_snapshot(
    error = TRUE,
    recipe(Job ~ Age, data = credit_data) |>
      step_svmsmote(Job) |>
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  skip_if_not_installed("kernlab")

  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(class ~ x + y, data = circle_example) |>
      step_svmsmote(class, seed = seed) |>
      prep() |>
      bake(new_data = NULL) |>
      pull(x)
  }

  run_1 <- step_with_seed(seed = 1234)
  run_2 <- step_with_seed(seed = 1234)
  run_3 <- step_with_seed(seed = 12345)

  expect_equal(run_1, run_2)
  expect_false(identical(run_1, run_3))
})

test_that("test tidy()", {
  skip_if_not_installed("kernlab")

  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(terms = "class", id = "")
  trained <- tibble(terms = "class", id = "")

  expect_equal(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("ratio value works when oversampling", {
  skip_if_not_installed("kernlab")

  res1 <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class) |>
    prep() |>
    bake(new_data = NULL)

  res1.5 <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class, over_ratio = 0.5) |>
    prep() |>
    bake(new_data = NULL)

  expect_true(all(table(res1$class) == max(table(circle_example$class))))
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    max(table(circle_example$class)) * c(0.5, 1)
  )
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
  skip_if_not_installed("kernlab")

  circle_example_alt_levels <- list()
  for (i in 1:4) {
    circle_example_alt_levels[[i]] <- circle_example
  }

  for (i in c(2, 4)) {
    levels(circle_example_alt_levels[[i]]$class) <-
      rev(levels(circle_example_alt_levels[[i]]$class))
  }

  for (i in c(3, 4)) {
    circle_example_alt_levels[[i]]$class <-
      factor(
        x = circle_example_alt_levels[[i]]$class,
        levels = rev(levels(circle_example_alt_levels[[i]]$class))
      )
  }

  for (i in 1:4) {
    rec_p <- recipe(class ~ x + y, data = circle_example_alt_levels[[i]]) |>
      step_svmsmote(class) |>
      prep()

    expect_equal(
      levels(circle_example_alt_levels[[i]]$class),
      rec_p$levels$class$values
    )
    expect_equal(
      levels(circle_example_alt_levels[[i]]$class),
      levels(bake(rec_p, new_data = NULL)$class)
    )
  }
})

test_that("ordering of newly generated points are right", {
  skip_if_not_installed("kernlab")

  res <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    res[seq_len(nrow(circle_example)), ],
    as_tibble(circle_example[, c("x", "y", "class")])
  )
})

test_that("non-predictor variables are ignored", {
  skip_if_not_installed("kernlab")

  res <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_svmsmote(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    c(circle_example$id, rep(NA, nrow(res) - nrow(circle_example))),
    as.character(res$id)
  )
})

test_that("id variables don't turn predictors to factors (#56)", {
  skip_if_not_installed("kernlab")

  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_svmsmote(class) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})

test_that("tunable", {
  rec <- recipe(~., data = mtcars) |>
    step_svmsmote(all_predictors())
  rec_param <- tunable.step_svmsmote(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("indicator_column adds logical column marking synthetic rows", {
  skip_if_not_installed("kernlab")

  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class, indicator_column = ".new_row") |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_true(".new_row" %in% names(res))
  expect_type(res$.new_row, "logical")
  expect_equal(sum(!res$.new_row), nrow(circle_example))
  expect_gt(sum(res$.new_row), 0L)
})

test_that("indicator_column bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_svmsmote(class, indicator_column = 1)
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_svmsmote(class, indicator_column = "") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y, data = circle_example) |>
      step_svmsmote(class, indicator_column = "x") |>
      prep()
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_svmsmote(over_ratio = "yes") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_svmsmote(neighbors = TRUE) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_svmsmote(seed = TRUE)
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_svmsmote(
      all_predictors(),
      over_ratio = hardhat::tune(),
      neighbors = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("unused outcome levels are skipped with a warning (#238)", {
  skip_if_not_installed("kernlab")

  circle_example$class <- factor(
    circle_example$class,
    levels = c(levels(circle_example$class), "unused")
  )

  expect_snapshot(
    res <- recipe(class ~ x + y, data = circle_example) |>
      step_svmsmote(class) |>
      prep() |>
      bake(new_data = NULL)
  )

  expect_gt(nrow(res), 0)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("kernlab")

  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class, skip = FALSE) |>
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
  rec <- step_svmsmote(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_svmsmote(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_svmsmote(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("kernlab")

  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  skip_if_not_installed("kernlab")

  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_svmsmote(class, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 1))), 1L)
})
