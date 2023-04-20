library(testthat)
library(recipes)
library(dplyr)
library(modeldata)

test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_tomek(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_tomek(class, skip = FALSE) %>%
    add_role(class, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = circle_example, verbose = FALSE)

  expect_error(bake(trained, new_data = circle_example[, -3]),
               class = "new_data_missing_column")
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_tomek(class)
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(error = TRUE,
    rec %>%
      step_smote(x) %>%
      prep()
  )
  # Multiple variable check
  expect_snapshot(error = TRUE,
    rec %>%
      step_smote(class, id) %>%
      prep()
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_snapshot(error = TRUE,
    recipe(~., data = df_char) %>%
      step_tomek(x) %>%
      prep()
  )
})

test_that("NA in response", {
  data(credit_data)
  credit_data0 <- credit_data
  credit_data0[1, 1] <- NA

  expect_snapshot(error = TRUE,
    recipe(Status ~ Age, data = credit_data0) %>%
      step_tomek(Status) %>%
      prep()
  )
})

test_that("test tidy()", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_tomek(class, id = "")

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
      step_tomek(class) %>%
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

test_that("id variables are ignored", {
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_tomek(class) %>%
    prep()

  expect_equal(ncol(bake(rec_id, new_data = NULL)), 4)
})


test_that("id variables don't turn predictors to factors", {
  # https://github.com/tidymodels/themis/issues/56
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_tomek(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})


test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_tomek(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_tomek(rec)

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
  rec <- step_tomek(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
