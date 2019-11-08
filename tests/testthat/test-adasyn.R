library(testthat)
library(recipes)
library(dplyr)

context("adasyn")

credit_data2 <- select(credit_data[1:100, ], Status, Time, Price)

rec <- recipe(~ ., data = credit_data2)

test_that("basic usage", {
  rec1 <- rec %>%
    step_adasyn(Status, id = "")

  untrained <- tibble(
    terms = "Status",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec1, number = 1))

  rec1_p <- prep(rec1, training = credit_data2, retain = TRUE)

  trained <- tibble(
    terms = "Status",
    id = ""
  )

  expect_equal(trained, tidy(rec1_p, number = 1))


  tr_xtab <- table(juice(rec1_p)$Status, useNA = "no")
  te_xtab <- table(bake(rec1_p, new_data = credit_data2)$Status, useNA = "no")
  og_xtab <- table(credit_data2$Status, useNA = "no")

  expect_gt(
    tr_xtab[["bad"]],
    og_xtab[["bad"]]
  )

  expect_equal(
    tr_xtab[["good"]],
    og_xtab[["good"]]
  )

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1, training = credit_data2), NA)
})

test_that("step_adasyn errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe(~ ., data = df_char) %>%
      step_adasyn(x) %>%
      prep(),
    "should be numeric"
  )
})

test_printing(step_adasyn)
test_bad_data(step_adasyn)
test_no_skipping(step_adasyn)
