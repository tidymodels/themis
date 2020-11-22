library(testthat)
library(recipes)
library(dplyr)
set.seed(1234)

context("ROSE")

test_that("minority_prop value", {
  rec <- recipe(~., data = circle_example)
  rec21 <- rec %>%
    step_rose(class, minority_prop = 0.1)

  rec22 <- rec %>%
    step_rose(class, minority_prop = 0.2)

  rec21_p <- prep(rec21, training = circle_example)
  rec22_p <- prep(rec22, training = circle_example)

  tr_xtab1 <- table(bake(rec21_p, new_data = NULL)$class, useNA = "no")
  tr_xtab2 <- table(bake(rec22_p, new_data = NULL)$class, useNA = "no")

  expect_equal(sum(tr_xtab1), sum(tr_xtab2))

  expect_lt(tr_xtab1[["Circle"]], tr_xtab2[["Circle"]])
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) %>%
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

test_basic_usage(step_rose)
test_printing(step_rose)
test_bad_data(step_rose)
test_no_skipping(step_rose)
test_character_error(step_rose)
test_na_response(step_rose)
test_seed(step_rose)
test_tidy(step_rose)
test_2_class_only(step_rose)
test_factor_level_memory(step_rose)
test_id_variables_are_ignores(step_rose)
