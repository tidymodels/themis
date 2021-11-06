library(testthat)
library(recipes)
library(dplyr)

context("adasyn")

test_that("tunable", {
  rec <- recipe(~., data = iris) %>%
    step_adasyn(all_predictors(), under_ratio = 1)
  rec_param <- tunable.step_adasyn(rec$steps[[1]])
  expect_equal(rec_param$name, c("over_ratio", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("errors if there isn't enough data", {
  iris4 <- iris

  iris4$Species <- as.character(iris4$Species)
  iris4$Species[1] <- "dummy"
  iris4$Species <- as.factor(iris4$Species)

  expect_error(
    recipe(~., data = iris4) %>%
      step_adasyn(Species) %>%
      prep(),
    "Not enough observations"
  )
})

test_that("basic usage", {
  rec1 <- recipe(~., data = circle_example) %>%
    step_adasyn(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_warning(prep(rec1, training = circle_example), NA)
})

test_that("printing", {
  rec <- recipe(~., data = circle_example) %>%
    step_adasyn(class)
  expect_output(print(rec))
  expect_output(
    prep(
      rec,
      training = circle_example,
      retain = TRUE,
      verbose = TRUE
    ))
})

test_that("bad data", {
  iris2 <- iris[-c(1:45), ]
  iris2$Species2 <- sample(iris2$Species)
  iris2$Species3 <- as.character(sample(iris2$Species))

  rec <- recipe(~., data = iris2)
  # numeric check
  expect_error(
    rec %>%
      step_adasyn(Sepal.Width) %>%
      prep(retain = TRUE)
  )
  # Multiple variable check
  expect_error(
    rec %>%
      step_adasyn(Species, Species2) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
  # character check
  expect_error(
    rec %>%
      step_adasyn(Species3) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_error(
    recipe(~., data = df_char) %>%
      step_adasyn(x) %>%
      prep(),
    "should be numeric"
  )
})

test_that("NA in response", {
  iris2 <- iris[-c(1:45), ]
  iris2$Species[seq(6, 96, by = 5)] <- NA
  # NA check
  expect_error(
    recipe(~., data = iris2) %>%
      step_adasyn(Species) %>%
      prep(strings_as_factors = FALSE, retain = TRUE)
  )
})

test_that("`seed` produces identical sampling", {
  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(~., data = circle_example) %>%
      step_adasyn(class, seed = seed) %>%
      prep(training = circle_example, retain = TRUE) %>%
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
  rec <- recipe(~., data = circle_example) %>%
    step_adasyn(class, id = "")

  rec_p <- prep(rec, training = circle_example, retain = TRUE)

  untrained <- tibble(
    terms = "class",
    id = ""
  )

  trained <- tibble(
    terms = "class",
    id = ""
  )

  expect_equivalent(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("ratio value works when oversampling", {
  res1 <- recipe(~., data = circle_example) %>%
    step_adasyn(class) %>%
    prep() %>%
    bake(new_data = NULL)

  res1.5 <- recipe(~., data = circle_example) %>%
    step_adasyn(class, over_ratio = 0.5) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(table(res1$class) == max(table(circle_example$class))))
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    max(table(circle_example$class)) * c(0.5, 1)
  )
})

test_that("allows multi-class", {
  data <- rename(iris, class = Species)
  expect_error(
    recipe(~., data = data) %>%
      step_adasyn(class) %>%
      prep(),
    NA
  )
})

test_that("majority classes are ignored if there is more than 1", {
  rec1_p2 <- recipe(~., data = iris[-c(51:75), ]) %>%
    step_adasyn(Species) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(max(table(rec1_p2$Species)) <= 50))
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
      factor(x = circle_example_alt_levels[[i]]$class,
             levels = rev(levels(circle_example_alt_levels[[i]]$class)))
  }

  for (i in 1:4) {
    rec_p <- recipe(~., data = circle_example_alt_levels[[i]]) %>%
      step_adasyn(class) %>%
      prep(training = circle_example_alt_levels[[i]])

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
  res <- recipe(~., data = circle_example) %>%
    step_adasyn(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    res[seq_len(nrow(circle_example)), ],
    as_tibble(circle_example)
  )
})

test_that("non-predictor variables are ignored", {
  circle_example2 <- circle_example %>%
    mutate(id = as.character(row_number())) %>%
    as_tibble()

  res <- recipe(class ~ ., data = circle_example2) %>%
    update_role(id, new_role = "id") %>%
    step_adasyn(class) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(
    c(circle_example2$id, rep(NA, nrow(res) - nrow(circle_example2))),
    as.character(res$id)
  )
})
