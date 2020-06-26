
<!-- README.md is generated from README.Rmd. Please edit that file -->

# themis

<!-- badges: start -->

[![R build
status](https://github.com/tidymodels/themis/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/themis/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/themis/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/themis?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/themis)](https://CRAN.R-project.org/package=themis)
[![Downloads](http://cranlogs.r-pkg.org/badges/themis)](https://CRAN.R-project.org/package=themis)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

**themis** contain extra steps for the
[`recipes`](https://CRAN.R-project.org/package=recipes) package for
dealingwith unbalanced data. The name **themis** is that of the [ancient
Greek
god](https://thishollowearth.wordpress.com/2012/07/02/god-of-the-week-themis/)
who is typically depicted with a balance.

![](https://thishollowearth.files.wordpress.com/2012/07/themis.jpg)

## Installation

You can install the released version of themis from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("themis")
```

Install the development version from GitHub with:

``` r
require("devtools")
install_github("tidymodels/themis")
```

## Example

Following is a example of using the
[SMOTE](https://jair.org/index.php/jair/article/view/10302/24590)
algorithm to deal with unbalanced data

``` r
library(recipes)
library(modeldata)
library(themis)

data(okc)

sort(table(okc$Class, useNA = "always"))
#> 
#>  <NA>  stem other 
#>     0  9539 50316

ds_rec <- recipe(Class ~ age + height, data = okc) %>%
  step_meanimpute(all_predictors()) %>%
  step_smote(Class) %>%
  prep()

sort(table(juice(ds_rec)$Class, useNA = "always"))
#> 
#>  <NA>  stem other 
#>     0 50316 50316
```

## Methods

Below is some unbalanced data. Used for examples latter.

``` r
example_data <- data.frame(class = letters[rep(1:5, 1:5 * 10)],
                           x = rnorm(150))

library(ggplot2)

example_data %>%
  ggplot(aes(class)) +
  geom_bar()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Upsample / Over-sampling

The following methods all share the tuning parameter `over_ratio`, which
is the ratio of the majority-to-minority frequencies.

| name                                                            | function                  | Multi-class          |
| --------------------------------------------------------------- | ------------------------- | -------------------- |
| Random minority over-sampling with replacement                  | `step_upsample()`         | :heavy\_check\_mark: |
| Synthetic Minority Over-sampling Technique                      | `step_smote()`            | :heavy\_check\_mark: |
| Borderline SMOTE-1                                              | `step_bsmote(method = 1)` | :heavy\_check\_mark: |
| Borderline SMOTE-2                                              | `step_bsmote(method = 2)` | :heavy\_check\_mark: |
| Adaptive synthetic sampling approach for imbalanced learning    | `step_adasyn()`           | :heavy\_check\_mark: |
| Generation of synthetic data by Randomly Over Sampling Examples | `step_rose()`             |                      |

By setting `over_ratio = 1` you bring the number of samples of all
minority classes equal to 100% of the majority class.

``` r
recipe(~., example_data) %>%
  step_upsample(class, over_ratio = 1) %>%
  prep() %>%
  juice() %>%
  ggplot(aes(class)) +
  geom_bar()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

and by setting `over_ratio = 0.5` we upsample any minority class with
less samples then 50% of the majority up to have 50% of the majority.

``` r
recipe(~., example_data) %>%
  step_upsample(class, over_ratio = 0.5) %>%
  prep() %>%
  juice() %>%
  ggplot(aes(class)) +
  geom_bar()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Downsample / Under-sampling

Most of the the following methods all share the tuning parameter
`under_ratio`, which is the ratio of the minority-to-majority
frequencies.

| name                                            | function            | Multi-class          | under\_ratio         |
| ----------------------------------------------- | ------------------- | -------------------- | -------------------- |
| Random majority under-sampling with replacement | `step_downsample()` | :heavy\_check\_mark: | :heavy\_check\_mark: |
| NearMiss-1                                      | `step_nearmiss()`   | :heavy\_check\_mark: | :heavy\_check\_mark: |
| Extraction of majority-minority Tomek links     | `step_tomek()`      |                      |                      |

By setting `under_ratio = 1` you bring the number of samples of all
majority classes equal to 100% of the minority class.

``` r
recipe(~., example_data) %>%
  step_downsample(class, under_ratio = 1) %>%
  prep() %>%
  juice() %>%
  ggplot(aes(class)) +
  geom_bar()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

and by setting `under_ratio = 2` we downsample any majority class with
more then 200% samples of the minority class down to have to 200%
samples of the minority.

``` r
recipe(~., example_data) %>%
  step_downsample(class, under_ratio = 2) %>%
  prep() %>%
  juice() %>%
  ggplot(aes(class)) +
  geom_bar()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

  - For questions and discussions about tidymodels packages, modeling,
    and machine learning, [join us on RStudio
    Community](https://rstd.io/tidymodels-community).

  - If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/themis/issues).

  - Either way, learn how to create and share a
    [reprex](https://rstd.io/reprex) (a minimal, reproducible example),
    to clearly communicate about your code.

  - Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
