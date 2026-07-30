test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class)

  res <- prep(rec1) |> bake(new_data = NULL)

  expect_all_equal(
    as.vector(table(res$class)),
    min(table(circle_example$class))
  )
  expect_no_warning(prep(rec1))
})

test_that("works with a single predictor", {
  skip_if_not_installed("modeldata")

  data("hpc_data", package = "modeldata")

  expect_no_error(
    recipe(class ~ compounds, data = hpc_data) |>
      step_cluster_centroids(all_outcomes()) |>
      prep() |>
      bake(NULL)
  )
})

test_that("ratio value works when undersampling", {
  res1.5 <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class, under_ratio = 1.5) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    min(table(circle_example$class)) * c(1, 1.5)
  )
})

test_that("results are reproducible for a given seed", {
  bake_it <- function(seed) {
    recipe(class ~ x + y, data = circle_example) |>
      step_cluster_centroids(class, seed = seed) |>
      prep() |>
      bake(new_data = NULL)
  }

  expect_equal(bake_it(42), bake_it(42))
})

test_that("hard voting keeps observations from the data", {
  res <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class, voting = "hard") |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(
    nrow(dplyr::setdiff(
      res,
      tibble::as_tibble(circle_example[c(
        "x",
        "y",
        "class"
      )])
    )),
    0L
  )
})

test_that("skipping means baking has no effect", {
  rec_p <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class) |>
    prep()

  expect_equal(
    table(bake(rec_p, new_data = circle_example)$class, useNA = "no"),
    table(circle_example$class, useNA = "no")
  )
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_cluster_centroids(x) |>
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec |>
      step_cluster_centroids(class, id) |>
      prep()
  )
})

test_that("test tidy()", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class, id = "")

  rec_p <- prep(rec)

  expected <- tibble(terms = "class", id = "")

  expect_equal(expected, tidy(rec, number = 1))
  expect_equal(expected, tidy(rec_p, number = 1))
})

test_that("distance_with allows non-numeric columns to be present", {
  df_mixed <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(20, 3, 1)),
    y = c(rnorm(50, 0, 1), rnorm(20, 3, 1)),
    name = c(rep("alice", 50), rep("bob", 20)),
    class = factor(c(rep("majority", 50), rep("minority", 20)))
  )

  expect_no_error(
    recipe(class ~ ., data = df_mixed) |>
      step_cluster_centroids(class, distance_with = c(x, y)) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("id variables are ignored", {
  rec_id <- recipe(class ~ ., data = circle_example) |>
    update_role(id, new_role = "id") |>
    step_cluster_centroids(class) |>
    prep()

  res <- bake(rec_id, new_data = NULL)

  expect_equal(ncol(res), 4)
  expect_type(res$x, "double")
})

test_that("non-predictor columns are NA for soft voting but kept for hard", {
  make_rec <- function(voting) {
    recipe(class ~ ., data = circle_example) |>
      update_role(id, new_role = "id") |>
      step_cluster_centroids(class, voting = voting) |>
      prep() |>
      bake(new_data = NULL)
  }

  soft <- make_rec("soft")
  hard <- make_rec("hard")

  expect_equal(sum(is.na(soft$id)), sum(soft$class == "Rest"))
  expect_equal(sum(is.na(hard$id)), 0L)
})

test_that("case weights error for soft voting", {
  df_wts <- circle_example
  df_wts$wts <- hardhat::frequency_weights(rep(1L, nrow(df_wts)))

  expect_snapshot(
    error = TRUE,
    recipe(class ~ x + y + wts, data = df_wts) |>
      step_cluster_centroids(class) |>
      prep() |>
      bake(new_data = NULL)
  )
})

test_that("allows multi-class", {
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")

  res <- recipe(
    species ~ bill_length_mm + bill_depth_mm,
    data = penguins
  ) |>
    step_impute_mean(all_predictors()) |>
    step_cluster_centroids(species) |>
    prep() |>
    bake(new_data = NULL)

  expect_all_equal(as.vector(table(res$species)), min(table(penguins$species)))
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
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
      step_cluster_centroids(class) |>
      prep()

    expect_equal(
      levels(circle_example_alt_levels[[i]]$class),
      levels(bake(rec_p, new_data = NULL)$class)
    )
  }
})

test_that("unused outcome levels are skipped with a warning", {
  circle_example$class <- factor(
    circle_example$class,
    levels = c(levels(circle_example$class), "unused")
  )

  expect_snapshot(
    res <- recipe(class ~ x + y, data = circle_example) |>
      step_cluster_centroids(class) |>
      prep() |>
      bake(new_data = NULL)
  )

  expect_gt(nrow(res), 0)
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_cluster_centroids(under_ratio = "yes") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_cluster_centroids(voting = "medium")
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_cluster_centroids(seed = TRUE)
  )
})

test_that("tunable", {
  rec <- recipe(~., data = mtcars) |>
    step_cluster_centroids(all_predictors())
  rec_param <- tunable.step_cluster_centroids(rec$steps[[1]])
  expect_equal(rec_param$name, "under_ratio")
  expect_all_equal(rec_param$source, "recipe")
  expect_type(rec_param$call_info, "list")
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_cluster_centroids(
      all_predictors(),
      under_ratio = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("step_cluster_centroids() accepts a named `under_ratio` vector (#323)", {
  set.seed(1)
  df <- data.frame(
    x = rnorm(70),
    y = rnorm(70),
    class = factor(c(rep("a", 10), rep("b", 20), rep("c", 40)))
  )

  res <- recipe(class ~ x + y, data = df) |>
    step_cluster_centroids(class, under_ratio = c(c = 2)) |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(as.numeric(table(res$class)), c(10, 20, 20))
})

test_that("cluster_centroids() with a constant vector matches the scalar (#323)", {
  set.seed(1)
  df <- data.frame(
    x = rnorm(70),
    y = rnorm(70),
    class = factor(c(rep("a", 10), rep("b", 20), rep("c", 40)))
  )

  set.seed(2)
  res_vec <- cluster_centroids(
    df,
    "class",
    under_ratio = c(a = 1.5, b = 1.5, c = 1.5)
  )
  set.seed(2)
  res_scalar <- cluster_centroids(df, "class", under_ratio = 1.5)

  expect_equal(res_vec, res_scalar)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class, skip = FALSE) |>
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
  rec <- step_cluster_centroids(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_cluster_centroids(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_cluster_centroids(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  rec <- recipe(class ~ x + y, data = circle_example) |>
    step_cluster_centroids(class, skip = FALSE) |>
    prep()

  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 0))), 0L)
  expect_identical(nrow(bake(rec, new_data = slice(circle_example, 1))), 1L)
})
