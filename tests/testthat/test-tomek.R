library(testthat)
library(recipes)
library(dplyr)

test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_tomek(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_tomek(class)
  expect_output(print(rec))
  expect_output(prep(rec, verbose = TRUE))
})

test_that("bad data", {

  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_error(
    rec %>%
      step_smote(x) %>%
      prep(),
    regexp = "should be a factor variable."
  )
  # Multiple variable check
  expect_error(
    rec %>%
      step_smote(class, id) %>%
      prep(),
    regexp = "Please select a single factor variable."
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_error(
    recipe(~., data = df_char) %>%
      step_tomek(x) %>%
      prep(),
    "should be numeric"
  )
})

test_that("NA in response", {
  data(credit_data)
  credit_data0 <- credit_data
  credit_data0[1, 1] <- NA

  expect_error(
    recipe(Status ~ Age, data = credit_data0) %>%
      step_tomek(Status) %>%
      prep(),
    regexp = "NAs found ind: Status."
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

test_that("only except 2 classes", {
  df_char <- data.frame(
    x = factor(1:3),
    stringsAsFactors = FALSE
  )

  expect_error(
    recipe(~., data = df_char) %>%
      step_tomek(x) %>%
      prep(),
    "only have 2 levels."
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
      factor(x = circle_example_alt_levels[[i]]$class,
             levels = rev(levels(circle_example_alt_levels[[i]]$class)))
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
