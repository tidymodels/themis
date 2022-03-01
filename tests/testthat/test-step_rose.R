library(testthat)
library(recipes)
library(dplyr)
library(modeldata)

set.seed(1234)

test_that("minority_prop value", {
  rec <- recipe(class ~ x + y, data = circle_example)
  rec21 <- rec %>%
    step_rose(class, minority_prop = 0.1)

  rec22 <- rec %>%
    step_rose(class, minority_prop = 0.2)

  rec21_p <- prep(rec21)
  rec22_p <- prep(rec22)

  tr_xtab1 <- table(bake(rec21_p, new_data = NULL)$class, useNA = "no")
  tr_xtab2 <- table(bake(rec22_p, new_data = NULL)$class, useNA = "no")

  expect_equal(sum(tr_xtab1), sum(tr_xtab2))

  expect_lt(tr_xtab1[["Circle"]], tr_xtab2[["Circle"]])
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_rose(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_rose(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("row matching works correctly #36", {
  expect_error(
    recipe(class ~ ., data = circle_example) %>%
      step_rose(class, over_ratio = 1.2) %>%
      prep(),
    NA
  )

  expect_error(
    recipe(class ~ ., data = circle_example) %>%
      step_rose(class, over_ratio = 0.8) %>%
      prep(),
    NA
  )

  expect_error(
    recipe(class ~ ., data = circle_example) %>%
      step_rose(class, over_ratio = 1.7) %>%
      prep(),
    NA
  )
})

test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_rose(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_rose(class)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec, verbose = TRUE))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(error = TRUE,
    rec %>%
      step_rose(x) %>%
      prep()
  )
  # Multiple variable check
  expect_snapshot(error = TRUE,
    rec %>%
      step_rose(class, id) %>%
      prep()
  )
})

test_that("NA in response", {
  data(credit_data)
  credit_data0 <- credit_data
  credit_data0[1, 1] <- NA

  expect_snapshot(error = TRUE,
    recipe(Status ~ Age, data = credit_data0) %>%
      step_rose(Status) %>%
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(class ~ x + y, data = circle_example) %>%
      step_rose(class, seed = seed) %>%
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
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_rose(class, id = "")

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

test_that("only except 2 classes", {
  df_char <- data.frame(
    x = factor(1:3),
    stringsAsFactors = FALSE
  )

  expect_snapshot(error = TRUE,
    recipe(~., data = df_char) %>%
      step_rose(x) %>%
      prep()
  )
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
    rec_p <- recipe(class ~ x + y, data = circle_example_alt_levels[[i]]) %>%
      step_rose(class) %>%
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

test_that("non-predictor variables are ignored", {
  circle_example2 <- circle_example %>%
    mutate(id = as.character(row_number())) %>%
    as_tibble()

  res <- recipe(class ~ ., data = circle_example2) %>%
    update_role(id, new_role = "id") %>%
    step_rose(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    c(circle_example2$id, rep(NA, nrow(res) - nrow(circle_example2))),
    as.character(res$id)
  )
})


test_that("id variables don't turn predictors to factors", {
  # https://github.com/tidymodels/themis/issues/56
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_rose(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})


test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_rose(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_rose(rec)

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
  rec <- step_rose(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
