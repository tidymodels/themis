
test_that("order doesn't matter", {
  df <- data.frame(
    target = rep(c("Yes", "No"), c(10, 50)),
    x = rep(c(1, 2, 3), c(9, 2, 49))
  )
  expect_equal(c(10, 11), themis:::tomek_impl(df, "target"))

  df <- data.frame(
    target = rep(c("Yes", "No"), c(50, 10)),
    x = rep(c(1, 2, 3), c(49, 2, 9))
  )
  expect_equal(c(50, 51), themis:::tomek_impl(df, "target"))
})


test_that("tomek() interfaces correctly", {
  circle_example_num <- circle_example[, 1:3]

  expect_error(tomek(circle_example_num, var = "class"), NA)

  expect_snapshot(error = TRUE,
                  tomek(circle_example_num, var = "Class")
  )

  expect_snapshot(error = TRUE,
                  tomek(circle_example_num, var = c("class", "x"))
  )

  expect_snapshot(error = TRUE,
                  tomek(circle_example_num, var = "x")
  )

  circle_example0 <- circle_example_num
  circle_example0[1, 1] <- NA

  expect_snapshot(error = TRUE,
                  tomek(circle_example0, var = "class")
  )
})

test_that("ordering of columns shouldn't matter", {
  data("credit_data", package = "modeldata")

  credit_data0 <- credit_data %>%
    filter(!is.na(Job)) %>%
    select(Job, Time, Age, Expenses)

  expect_error(
    tomek(credit_data0, "Job"),
    NA
  )
})
