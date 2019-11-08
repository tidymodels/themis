library(testthat)
library(recipes)
library(dplyr)

context("SMOTE")

iris2 <- iris[-c(1:30), ]

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_smote(Species, id = "")

  rec1_p <- prep(rec1, training = iris2, retain = TRUE)

  trained <- tibble(
    terms = "Species",
    id = ""
  )

  tr_xtab <- table(juice(rec1_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec1_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_length(unique(tr_xtab[["setosa"]]), 1)

  expect_equal(te_xtab, og_xtab)

  expect_warning(prep(rec1, training = iris2), NA)
})

test_that("majority classes are ignored if there is more than 1", {
  rec1_p2 <- rec %>%
    step_smote(Species, id = "") %>%
    prep() %>%
    juice()

  expect_true(all(max(table(rec1_p2$Species)) <= 50))
})

test_that("over_ratio value", {
  rec2 <- rec %>%
    step_smote(tidyselect::matches("Species$"), over_ratio = 0.7)

  rec2_p <- prep(rec2, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec2_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(max(tr_xtab) * 0.7, min(tr_xtab))

  expect_equal(te_xtab, og_xtab)
})

test_that("all minority classes are upsampled", {
  iris3 <- iris[-c(1:25, 51:75), ]

  out <- recipe(~ ., data = iris3) %>%
    step_smote(Species) %>%
    prep() %>%
    juice()

  expect_equal(as.numeric(table(out$Species)), c(50, 50, 50))
})

test_printing(step_smote)
test_bad_data(step_smote)
test_no_skipping(step_smote)
test_character_error(step_smote)
test_na_response(step_smote)
test_seed(step_smote)
test_tidy(step_smote)
