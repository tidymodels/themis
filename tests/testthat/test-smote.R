library(testthat)
library(recipes)
library(dplyr)

context("SMOTE")

iris2 <- iris[-c(1:30), ]

rec <- recipe(~ ., data = iris2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_smote(Species, id = "")

  untrained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec1, number = 1))

  rec1_p <- prep(rec1, training = iris2, retain = TRUE)

  trained <- tibble(
    terms = "Species",
    id = ""
  )

  expect_equal(trained, tidy(rec1_p, number = 1))


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

test_that("`seed` produces identical sampling", {

  smote_with_seed <- function(rec, seed = sample.int(10^5, 1)) {
    rec %>%
      step_smote(Species, seed = seed) %>%
      prep(training = iris2, retain = TRUE) %>%
      juice() %>%
      pull(Petal.Width)
  }

  petal_width_1 <- smote_with_seed(rec, seed = 1234)
  petal_width_2 <- smote_with_seed(rec, seed = 1234)
  petal_width_3 <- smote_with_seed(rec, seed = 12345)

  expect_equal(petal_width_1, petal_width_2)
  expect_false(identical(petal_width_1, petal_width_3))
})

test_that("checks are done to ensure step_smote errors if NA are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c(NA, 1))

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_smote(x) %>%
      prep(),
    "missing"
  )
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
