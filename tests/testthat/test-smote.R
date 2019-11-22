library(testthat)
library(recipes)
library(dplyr)

context("SMOTE")

iris2 <- iris[-c(1:30), ]

rec <- recipe(~ ., data = iris2)

test_that("all minority classes are upsampled", {
  iris3 <- iris[-c(1:25, 51:75), ]

  out <- recipe(~ ., data = iris3) %>%
    step_smote(Species) %>%
    prep() %>%
    juice()

  expect_equal(as.numeric(table(out$Species)), c(50, 50, 50))
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_smote(all_predictors(), under_ratio = 1)
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

test_basic_usage(step_smote)
test_printing(step_smote)
test_bad_data(step_smote)
test_no_skipping(step_smote)
test_character_error(step_smote)
test_na_response(step_smote)
test_seed(step_smote)
test_tidy(step_smote)
test_over_ratio(step_smote)
test_multiclass(step_smote)
test_multi_majority(step_smote)
