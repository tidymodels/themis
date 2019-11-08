library(testthat)
library(recipes)
library(dplyr)

set.seed(1234)

context("nearmiss")

iris2 <- iris[-c(51:75), ]

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_nearmiss(Species, id = "")

  rec1_p <- prep(rec1, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec1_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec1_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_length(unique(tr_xtab[["setosa"]]), 1)

  expect_equal(te_xtab, og_xtab)

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that("minority classes are ignored if there is more than 1", {

  rec1_p2 <- recipe(~., data = iris[-c(1:25, 51:75), ]) %>%
    step_nearmiss(Species, id = "") %>%
    prep() %>%
    juice()

  expect_true(all(max(table(rec1_p2$Species)) == 25))
})

test_that("under_ratio value", {
  rec2 <- rec %>%
    step_nearmiss(tidyselect::matches("Species$"), under_ratio = 1.5)

  rec2_p <- prep(rec2, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec2_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(floor(min(tr_xtab) * 1.5), max(tr_xtab))

  expect_equal(te_xtab, og_xtab)
})

test_that("tunable", {
  rec <-
    recipe(~ ., data = iris) %>%
    step_nearmiss(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_nearmiss(rec$steps[[1]])
  expect_equal(rec_param$name, c("under_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_printing(step_nearmiss)
test_bad_data(step_nearmiss)
test_no_skipping(step_nearmiss)
test_character_error(step_nearmiss)
test_na_response(step_nearmiss)
test_seed(step_nearmiss)
test_tidy(step_nearmiss)
