library(recipes)

test_that("checks are done to ensure step_smote errors if character are present", {
  df_char <- data.frame(x = factor(1:2),
                        y = c("A", "A"),
                        stringsAsFactors = FALSE)

  expect_error(
    recipe( ~ ., data = df_char) %>%
      step_smote(x) %>%
      prep(),
    "should be numeric"
  )
})
