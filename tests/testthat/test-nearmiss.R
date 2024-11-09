test_that("basic usage", {
  rec1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_nearmiss(class)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  expect_equal(sort(te_xtab), sort(og_xtab))

  expect_no_warning(prep(rec1))
})

test_that("bad data", {
  rec <- recipe(~., data = circle_example)
  # numeric check
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_nearmiss(x) %>%
      prep()
  )
  # Multiple variable check
  expect_snapshot(
    error = TRUE,
    rec %>%
      step_nearmiss(class, id) %>%
      prep()
  )
})

test_that("errors if character are present", {
  df_char <- data.frame(
    x = factor(1:2),
    y = c("A", "A"),
    stringsAsFactors = FALSE
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = df_char) %>%
      step_nearmiss(x) %>%
      prep()
  )
})

test_that("NA in response", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")

  expect_snapshot(
    error = TRUE,
    recipe(Job ~ Age, data = credit_data) %>%
      step_nearmiss(Job) %>%
      prep()
  )
})

test_that("test tidy()", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_nearmiss(class, id = "")

  rec_p <- prep(rec)

  untrained <- tibble(
    terms = "class",
    id = ""
  )

  trained <- tibble(
    terms = "class",
    id = ""
  )

  expect_equal(untrained, tidy(rec, number = 1))
  expect_equal(trained, tidy(rec_p, number = 1))
})

test_that("ratio value works when undersampling", {
  res1 <- recipe(class ~ x + y, data = circle_example) %>%
    step_nearmiss(class) %>%
    prep() %>%
    bake(new_data = NULL)

  res1.5 <- recipe(class ~ x + y, data = circle_example) %>%
    step_nearmiss(class, under_ratio = 1.5) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(table(res1$class) == min(table(circle_example$class))))
  expect_equal(
    sort(as.numeric(table(res1.5$class))),
    min(table(circle_example$class)) * c(1, 1.5)
  )
})

test_that("allows multi-class", {
  skip_if_not_installed("modeldata")

  data("credit_data", package = "modeldata")
  expect_no_error(
    recipe(Home ~ Age + Income + Assets, data = credit_data) %>%
      step_impute_mean(Income, Assets) %>%
      step_nearmiss(Home)
  )
})

test_that("minority classes are ignored if there is more than 1", {
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")
  rec1_p2 <- recipe(species ~ bill_length_mm + bill_depth_mm,
    data = penguins[-(1:84), ]
  ) %>%
    step_impute_mean(all_predictors()) %>%
    step_nearmiss(species) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_true(all(max(table(rec1_p2$species)) == 68))
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
      factor(
        x = circle_example_alt_levels[[i]]$class,
        levels = rev(levels(circle_example_alt_levels[[i]]$class))
      )
  }

  for (i in 1:4) {
    rec_p <- recipe(class ~ x + y, data = circle_example_alt_levels[[i]]) %>%
      step_nearmiss(class) %>%
      prep()

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

test_that("id variables are ignored", {
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_nearmiss(class, under_ratio = 1) %>%
    prep()

  expect_equal(ncol(bake(rec_id, new_data = NULL)), 4)
})

test_that("id variables don't turn predictors to factors", {
  # https://github.com/tidymodels/themis/issues/56
  rec_id <- recipe(class ~ ., data = circle_example) %>%
    update_role(id, new_role = "id") %>%
    step_nearmiss(class, under_ratio = 1) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(is.double(rec_id$x), TRUE)
  expect_equal(is.double(rec_id$y), TRUE)
})

test_that("tunable", {
  rec <- recipe(~., data = mtcars) %>%
    step_nearmiss(all_predictors())
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

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_nearmiss(over_ratio = "yes") %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_nearmiss(neighbors = TRUE) %>%
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) %>%
      step_nearmiss(seed = TRUE)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_nearmiss(class, skip = FALSE) %>%
    add_role(class, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  trained <- prep(rec, training = circle_example, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(trained, new_data = circle_example[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nearmiss(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_nearmiss(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_nearmiss(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ x + y, data = circle_example) %>%
    step_nearmiss(class)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) %>%
    step_nearmiss(
      all_predictors(),
      under_ratio = hardhat::tune(),
      neighbors = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
