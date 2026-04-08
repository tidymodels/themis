# Adaptive Synthetic Algorithm

Generates synthetic positive instances using ADASYN algorithm.

## Usage

``` r
adasyn(df, var, k = 5, over_ratio = 1)
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

- k:

  An integer. Number of nearest neighbor that are used to generate the
  new examples of the minority class.

- over_ratio:

  A numeric value for the ratio of the minority-to-majority frequencies.
  The default value (1) means that all other levels are sampled up to
  have the same frequency as the most occurring level. A value of 0.5
  would mean that the minority levels will have (at most)
  (approximately) half as many rows as the majority level.

## Value

A data.frame or tibble, depending on type of `df`.

## Details

All columns used in this function must be numeric with no missing data.

## References

He, H., Bai, Y., Garcia, E. and Li, S. 2008. ADASYN: Adaptive synthetic
sampling approach for imbalanced learning. Proceedings of IJCNN 2008.
(IEEE World Congress on Computational Intelligence). IEEE International
Joint Conference. pp.1322-1328.

## See also

[`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
for step function of this method

Other Direct Implementations:
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- adasyn(circle_numeric, var = "class")

res <- adasyn(circle_numeric, var = "class", k = 10)

res <- adasyn(circle_numeric, var = "class", over_ratio = 0.8)
```
