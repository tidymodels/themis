library(testthat)
library(recipes)
library(dplyr)

context("Down-sampling")

iris2 <- iris[-c(1:45), ]
iris2$Species[seq(6, 96, by = 5)] <- NA
iris2$Species2 <- sample(iris2$Species)
iris2$Species3 <- as.character(sample(iris2$Species))

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_downsample(tidyselect::matches("Species$"), id = "")

  rec1_p <- prep(rec1, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec1_p)$Species, useNA = "always")
  te_xtab <- table(bake(rec1_p, new_data = iris2)$Species, useNA = "always")
  og_xtab <- table(iris2$Species, useNA = "always")

  expect_equal(max(tr_xtab), 5)
  expect_equal(sum(is.na(juice(rec1_p)$Species)), 5)
  expect_equal(te_xtab, og_xtab)

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that("ratio deprecation", {

  expect_message(
    new_rec <-
      rec %>%
      step_downsample(tidyselect::matches("Species$"), ratio = 2),
    "argument is now deprecated"
  )
  expect_equal(new_rec$steps[[1]]$under_ratio, 2)
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
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

test_printing(step_downsample)
test_bad_data(step_downsample)
test_no_skipping(step_downsample)
test_seed(step_downsample)
test_tidy(step_downsample)
test_under_ratio(step_downsample)
