library(rlang)

test_printing <- function(step, data = NULL) {
  data <- data %||%
    circle_example
  rec <- recipe(~ ., data = data) %>%
    step(class)

  test_that("printing", {
    expect_output(print(rec))
    expect_output(prep(rec,
                       training = data,
                       retain = TRUE,
                       verbose = TRUE))
  })
}

test_bad_data <- function(step) {
  iris2 <- iris[-c(1:45), ]
  iris2$Species2 <- sample(iris2$Species)
  iris2$Species3 <- as.character(sample(iris2$Species))

  rec <- recipe(~ ., data = iris2)

  test_that("bad data", {
    # numeric check
    expect_error(
      rec %>%
        step(Sepal.Width) %>%
        prep(retain = TRUE)
    )
    # Multiple variable check
    expect_error(
      rec %>%
        step(Species, Species2) %>%
        prep(strings_as_factors = FALSE, retain = TRUE)
    )
    # character check
    expect_error(
      rec %>%
        step(Species3) %>%
        prep(strings_as_factors = FALSE, retain = TRUE)
    )
  })
}

test_na_response <- function(step) {
  iris2 <- iris[-c(1:45), ]
  iris2$Species[seq(6, 96, by = 5)] <- NA

  test_that("NA in response", {
    # NA check
    expect_error(
      recipe(~ ., data = iris2) %>%
        step(Species) %>%
        prep(strings_as_factors = FALSE, retain = TRUE)
    )
  })
}

test_no_skipping <- function(step, data = NULL) {
  data <- data %||%
    circle_example

  rec <- recipe(~ ., data = data) %>%
    step(class, skip = FALSE)

  rec_p <- prep(rec, training = data, retain = TRUE)

  tr_xtab <- table(juice(rec_p)$class, useNA = "always")
  te_xtab <- table(bake(rec_p, new_data = data)$class,
                   useNA = "always")

  test_that("no skipping", {
    expect_equal(te_xtab, tr_xtab)
  })
}

test_character_error <- function(step) {
  test_that("errors if character are present", {
    df_char <- data.frame(x = factor(1:2),
                          y = c("A", "A"),
                          stringsAsFactors = FALSE)

    expect_error(
      recipe(~ ., data = df_char) %>%
        step(x) %>%
        prep(),
      "should be numeric"
    )
  })
}

test_seed <- function(step, data = NULL) {
  data <- data %||% circle_example

  step_with_seed <- function(seed = sample.int(10^5, 1)) {
    recipe(~ ., data = data) %>%
      step(class, seed = seed) %>%
      prep(training = data, retain = TRUE) %>%
      juice() %>%
      pull(x)
  }

  test_that("`seed` produces identical sampling", {
    run_1 <- step_with_seed(seed = 1234)
    run_2 <- step_with_seed(seed = 1234)
    run_3 <- step_with_seed(seed = 12345)


    expect_equal(run_1, run_2)
    expect_false(identical(run_1, run_3))
  })
}

test_tidy <- function(step) {

  rec <- recipe(~ ., data = circle_example) %>%
    step(class, id = "")

  rec_p <- prep(rec, training = circle_example, retain = TRUE)

  untrained <- tibble(
    terms = "class",
    id = ""
  )

  trained <- tibble(
    terms = "class",
    id = ""
  )

  test_that("basic usage", {
    expect_equivalent(untrained, tidy(rec, number = 1))
    expect_equal(trained, tidy(rec_p, number = 1))
  })
}

test_under_ratio <- function(step) {
  res1 <- recipe(~ ., data = circle_example) %>%
    step(class) %>%
    prep() %>%
    juice()

  res1.5 <- recipe(~ ., data = circle_example) %>%
    step(class, under_ratio = 1.5) %>%
    prep() %>%
    juice()

  test_that("ratio value", {

    expect_true(all(table(res1$class) == min(table(circle_example$class))))
    expect_equal(
      sort(as.numeric(table(res1.5$class))),
      min(table(circle_example$class)) * c(1, 1.5)
      )
  })
}

test_over_ratio <- function(step, ...) {
  res1 <- recipe(~ ., data = circle_example) %>%
    step(class, ...) %>%
    prep() %>%
    juice()

  res1.5 <- recipe(~ ., data = circle_example) %>%
    step(class, over_ratio = 0.5) %>%
    prep() %>%
    juice()

  test_that("ratio value", {

    expect_true(all(table(res1$class) == max(table(circle_example$class))))
    expect_equal(
      sort(as.numeric(table(res1.5$class))),
      max(table(circle_example$class)) * c(0.5, 1)
    )
  })
}

test_basic_usage <- function(step, ...) {
  rec1 <- recipe(~ ., data = circle_example) %>%
    step(class, ...)

  rec1_p <- prep(rec1)

  te_xtab <- table(bake(rec1_p, new_data = circle_example)$class, useNA = "no")
  og_xtab <- table(circle_example$class, useNA = "no")

  test_that("basic usage", {
    expect_equal(sort(te_xtab), sort(og_xtab))

    expect_warning(prep(rec1, training = circle_example), NA)
  })
}

test_2_class_only <- function(step) {
  test_that("only except 2 classes", {
    df_char <- data.frame(x = factor(1:3),
                          stringsAsFactors = FALSE)

    expect_error(
      recipe(~ ., data = df_char) %>%
        step(x) %>%
        prep(),
      "only have 2 levels."
    )
  })
}

test_multiclass <- function(step, data = NULL) {

  data <- data %||%
    rename(iris, class = Species)
  test_that("allows multi-class", {
    expect_error(
      recipe(~ ., data = data) %>%
        step(class) %>%
        prep(),
      NA
    )
  })
}

test_multi_majority <- function(step, ...) {
  rec1_p2 <- recipe(~ ., data = iris[-c(51:75), ]) %>%
    step(Species, ...) %>%
    prep() %>%
    juice()

  test_that("majority classes are ignored if there is more than 1", {
    expect_true(all(max(table(rec1_p2$Species)) <= 50))
  })
}

test_multi_minority <- function(step, ...) {
  rec1_p2 <- recipe(~., data = iris[-c(1:25, 51:75), ]) %>%
    step(Species, ...) %>%
    prep() %>%
    juice()

  test_that("minority classes are ignored if there is more than 1", {
    expect_true(all(max(table(rec1_p2$Species)) == 25))
  })
}

test_factor_level_memory <- function(step, ...) {
  # Only checks for two level case

  circle_example_alt_levels <- list()
  for(i in 1:4) circle_example_alt_levels[[i]] <- circle_example

  # Checking for forgetting levels by majority/minor switching
  for(i in c(2, 4)){
    levels(circle_example_alt_levels[[i]]$class) <- rev(levels(circle_example_alt_levels[[i]]$class))
  }

  # Checking for forgetting levels by alphabetical switching
  for(i in c(3, 4)){
    circle_example_alt_levels[[i]]$class <- factor(circle_example_alt_levels[[i]]$class, levels = rev(levels(circle_example_alt_levels[[i]]$class)))
  }

  test_that("factor levels are not affected by alphabet ordering or class sizes", {
    for(i in 1:4){
      rec_p <- recipe(~ ., data = circle_example_alt_levels[[i]]) %>%
        step(class) %>%
        prep(training = circle_example_alt_levels[[i]])

      expect_equal(
        levels(circle_example_alt_levels[[i]]$class), # Original levels
        rec_p$levels$class$values                     # New levels
      )
      expect_equal(
        levels(circle_example_alt_levels[[i]]$class), # Original levels
        levels(juice(rec_p)$class)                    # New levels
      )
    }
  })

}

test_result_ordering <- function(step, ...) {
  res <- recipe(~ ., data = circle_example) %>%
    step(class, ...) %>%
    prep() %>%
    juice()

  test_that("ordering of newly generated points are right", {

    expect_equal(
      res[seq_len(nrow(circle_example)), ],
      as_tibble(circle_example)
    )

  })
}

test_id_variables_are_ignores <- function(step, ...) {
  circle_example2 <- circle_example %>%
    mutate(id = as.character(row_number())) %>%
    as_tibble()

  res <- recipe(class ~ ., data = circle_example2) %>%
    update_role(id, new_role = "id") %>%
    step(class, ...) %>%
    prep() %>%
    juice()

  test_that("non-predictor variables are ignored", {

    expect_equal(
      c(circle_example2$id, rep(NA, nrow(res) - nrow(circle_example2))),
      as.character(res$id)
    )

  })
}
