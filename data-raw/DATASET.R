## code to prepare `DATASET` dataset goes here
set.seed(1234)
library(dplyr)

circle_example <- data.frame(x =  runif(400, 1, 15),
                             y = runif(400, 1, 15)) %>%
  mutate(class = factor(sqrt((x - 8) ^ 2 + (y - 8) ^ 2) < 3, levels = c(TRUE, FALSE),  c("Circle", "Rest")))

usethis::use_data(circle_example, overwrite = TRUE)
