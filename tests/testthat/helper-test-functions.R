library(rlang)
test_printing <- function(step, data = NULL) {
  data <- data %||%
    dplyr::select(credit_data[1:100, ], class = Status, Time, Price)
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
