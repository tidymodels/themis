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
