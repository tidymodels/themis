library(testthat)
library(recipes)
library(dplyr)

context("tomek")

test_basic_usage(step_tomek)
test_printing(step_tomek)
test_bad_data(step_tomek)
test_no_skipping(step_tomek)
test_character_error(step_tomek)
test_na_response(step_tomek)
test_tidy(step_tomek)
test_2_class_only(step_tomek)
