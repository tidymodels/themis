library(testthat)
library(recipes)
library(dplyr)
library(modeldata)

test_that("errors if there isn't enough data", {
  data("credit_data")
  credit_data0 <- credit_data

  credit_data0$Status <- as.character(credit_data0$Status)
  credit_data0$Status[1] <- "dummy"
  credit_data0$Status <- as.factor(credit_data0$Status)

  expect_snapshot(error = TRUE,
    recipe(Status ~ Age, data = credit_data0) %>%
      step_smote(Status) %>%
      prep()
  )
})

test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_smote(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1), NA)
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
      step_smote(x) %>%
      prep()
  )
})

test_that("NA in response", {
  data(credit_data)

  expect_snapshot(error = TRUE,
    recipe(Job ~ Age, data = credit_data) %>%
      step_smote(Job) %>%
      prep()
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(class ~ x + y, data = circle_example) %>%
      step_smote(class, seed = seed) %>%
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
    step_smote(class, id = "")

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

test_that("ratio value works when oversampling", {
  res1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_smote(class) %>%
    prep() %>%
    bake(new_data = NULL)

  res1.5 <- recipe(class ~ x + y, data = circle_example) %>%
    step_smote(class, over_ratio = 0.5) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(table(res1$class) == max(table(circle_example$class))))
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    max(table(circle_example$class)) * c(0.5, 1)
  )
})

test_that("allows multi-class", {
  data("credit_data")
  expect_error(
    recipe(Home ~ Age + Income + Assets, data = credit_data) %>%
      step_impute_mean(Income, Assets) %>%
      step_smote(Home),
    NA
  )
})

test_that("majority classes are ignored if there is more than 1", {
  data("penguins")
  rec1_p2 <- recipe(species ~ bill_length_mm + bill_depth_mm,
    data = penguins[-(1:28), ]
  ) %>%
    step_impute_mean(all_predictors()) %>%
    step_smote(species) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(max(table(rec1_p2$species)) == 124))
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
      step_smote(class) %>%
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

test_that("ordering of newly generated points are right", {
  res <- recipe(class ~ x + y, data = circle_example) %>%
    step_smote(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    res[seq_len(nrow(circle_example)), ],
    as_tibble(circle_example[, c("x", "y", "class")])
  )
})

test_that("non-predictor variables are ignored", {
  res <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_smote(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    c(circle_example$id, rep(NA, nrow(res) - nrow(circle_example))),
    as.character(res$id)
  )
})

test_that("id variables don't turn predictors to factors", {
  # https://github.com/tidymodels/themis/issues/56
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_smote(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})

test_that("tunable", {
  rec <- recipe(~., data = mtcars) %>%
    step_smote(all_predictors())
  rec_param <- tunable.step_smote(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_smote(class, skip = FALSE) %>%
    add_role(class, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = circle_example, verbose = FALSE)

  expect_error(bake(trained, new_data = circle_example[, -3]),
               class = "new_data_missing_column")
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_smote(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_smote(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_smote(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_smote(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_smote(
      all_predictors(),
      over_ratio = hardhat::tune(),
      neighbors = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
