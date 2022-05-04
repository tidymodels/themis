library(testthat)
library(recipes)
library(dplyr)
library(modeldata)

set.seed(1234)

test_that("ratio deprecation", {
  expect_snapshot(error = TRUE,
    new_rec <- recipe(~., data = circle_example) %>%
      step_downsample(class, ratio = 2)
  )
})

test_that("tunable", {
  rec <- recipe(~., data = mtcars) %>%
    step_downsample(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_downsample(rec$steps[[1]])
  expect_equal(rec_param$name, c("under_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("basic usage", {
  rec1 <- recipe(~., data = circle_example) %>%
    step_downsample(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
})

test_that("printing", {
  rec <- recipe(~., data = circle_example) %>%
    step_downsample(class)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(error = TRUE,
    rec %>%
      step_downsample(x) %>%
      prep()
  )
  # Multiple variable check
  expect_snapshot(error = TRUE,
    rec %>%
      step_downsample(class, id) %>%
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(~., data = circle_example) %>%
      step_downsample(class, seed = seed) %>%
      prep() %>%
      bake(new_data = NULL) %>%
      pull(x)
  }

  run_1 <- step_with_seed(seed = 1234)
  run_2 <- step_with_seed(seed = 1234)
  run_3 <- step_with_seed(seed = 12345)

  expect_equal(run_1, run_2)
  expect_false(identical(run_1, run_3))
})

test_that("test tidy()", {
  rec <- recipe(~., data = circle_example) %>%
    step_downsample(class, id = "")

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

test_that("ratio value works when undersampling", {
  res1 <- recipe(~., data = circle_example) %>%
    step_downsample(class) %>%
    prep() %>%
    bake(new_data = NULL)

  res1.5 <- recipe(~., data = circle_example) %>%
    step_downsample(class, under_ratio = 1.5) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(table(res1$class) == min(table(circle_example$class))))
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    min(table(circle_example$class)) * c(1, 1.5)
  )
})

test_that("allows multi-class", {
  data("credit_data")
  expect_error(
    recipe(Home ~ Age + Income + Assets, data = credit_data) %>%
      step_impute_mean(Income, Assets) %>%
      step_downsample(Home),
    NA
  )
})

test_that("minority classes are ignored if there is more than 1", {
  data("penguins")
  rec1_p2 <- recipe(species ~ bill_length_mm + bill_depth_mm,
    data = penguins[-(1:84), ]
  ) %>%
    step_impute_mean(all_predictors()) %>%
    step_downsample(species) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(max(table(rec1_p2$species)) == 68))
})

test_that("factor levels are not affected by alphabet ordering or class sizes", {
  circle_example_alt_levels <- list()
  for (i in 1:4) circle_example_alt_levels[[i]] <- circle_example

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
    rec_p <- recipe(~., data = circle_example_alt_levels[[i]]) %>%
      step_downsample(class) %>%
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


test_that("id variables don't turn predictors to factors", {
  # https://github.com/tidymodels/themis/issues/56
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_downsample(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})


test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_downsample(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_downsample(rec)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_downsample(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
