set.seed(1234)
library(dplyr)

circle_example <- data.frame(
  x = runif(400, 1, 15),
  y = runif(400, 1, 15)
) %>%
  mutate(
    class = factor(
      x = sqrt((x - 8)^2 + (y - 8)^2) < 3,
      levels = c(TRUE, FALSE),
      labels = c("Circle", "Rest")
    ),
    id = as.character(row_number())
  )

usethis::use_data(circle_example, overwrite = TRUE)
