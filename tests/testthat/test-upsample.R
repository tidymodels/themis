library(testthat)
library(recipes)
library(dplyr)

context("Upsampling")

test_that("ratio deprecation", {
  expect_message(
    new_rec <- recipe(~ ., data = circle_example) %>%
      step_upsample(class, ratio = 2),
    "argument is now deprecated"
  )
  expect_equal(new_rec$steps[[1]]$over_ratio, 2)
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_upsample(all_predictors())
  rec_param <- tunable.step_upsample(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_basic_usage(step_upsample)
test_printing(step_upsample)
test_bad_data(step_upsample)
test_no_skipping(step_upsample)
test_seed(step_upsample)
test_tidy(step_upsample)
test_over_ratio(step_upsample)
test_multiclass(step_upsample)
test_multi_majority(step_upsample)
