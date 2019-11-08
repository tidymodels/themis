library(testthat)
library(recipes)
library(dplyr)

set.seed(1234)

context("bsmote")

iris2 <- iris[-c(51:75), ]

rec <- recipe(~ ., data = iris2)

rec3 <- rec %>%
  step_bsmote(tidyselect::matches("Species$"), skip = FALSE)

test_that("basic usage", {
  rec1 <- rec %>%
    step_bsmote(Species, id = "")

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
    step_bsmote(Species, id = "") %>%
    prep() %>%
    juice()

  expect_true(all(max(table(rec1_p2$Species)) <= 50))
})

test_that("over_ratio value", {
  rec2 <- rec %>%
    step_bsmote(tidyselect::matches("Species$"), over_ratio = 0.7)

  rec2_p <- prep(rec2, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec2_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(max(tr_xtab) * 0.7, min(tr_xtab))

  expect_equal(te_xtab, og_xtab)
})

test_that("all_neighbors argument", {
  rec2 <- rec %>%
    step_bsmote(tidyselect::matches("Species$"), all_neighbors = TRUE)

  rec2_p <- prep(rec2, training = iris2, retain = TRUE)

  tr_xtab <- table(juice(rec2_p)$Species, useNA = "no")
  te_xtab <- table(bake(rec2_p, new_data = iris2)$Species, useNA = "no")
  og_xtab <- table(iris2$Species, useNA = "no")

  expect_equal(te_xtab, og_xtab)
})

test_that("`seed` produces identical sampling", {

  bsmote_with_seed <- function(rec, seed = sample.int(10^5, 1)) {
    rec %>%
      step_bsmote(Species, seed = seed) %>%
      prep(training = iris2, retain = TRUE) %>%
      juice() %>%
      pull(Petal.Width)
  }

  petal_width_1 <- bsmote_with_seed(rec, seed = 1234)
  petal_width_2 <- bsmote_with_seed(rec, seed = 1234)
  petal_width_3 <- bsmote_with_seed(rec, seed = 12345)

  expect_equal(petal_width_1, petal_width_2)
  expect_false(identical(petal_width_1, petal_width_3))
})


test_that("step_bsmote errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_bsmote(x) %>%
      prep(),
    "should be numeric"
  )
})

test_that("checks are done to ensure step_bsmote errors if NA are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c(NA, 1))

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_bsmote(x) %>%
      prep(),
    "missing"
  )
})

test_that("all minority classes are upsampled", {
  iris3 <- iris[-c(51:75, 101:110), ]

  out <- recipe(~ ., data = iris3) %>%
    step_bsmote(Species) %>%
    prep() %>%
    juice()

  expect_equal(as.numeric(table(out$Species)), c(50, 50, 50))
})

test_that("errors if there isn't enough danger data", {
  iris4 <- iris[-c(1:10), ]

  expect_error(
    recipe(~ ., data = iris4) %>%
      step_bsmote(Species) %>%
      prep(),
    "Not enough danger observations"
  )
})

test_printing(step_bsmote,
              data = select(iris, class = Species, everything())[-c(51:75), ])
test_bad_data(step_bsmote)
test_no_skipping(step_bsmote,
                 data = select(iris, class = Species, everything())[-c(51:75), ])
