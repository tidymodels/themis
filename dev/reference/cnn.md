# Condensed Nearest Neighbors

Under-samples the majority classes by keeping only a consistent subset
of observations that correctly classifies the data using a
1-nearest-neighbor rule.

## Usage

``` r
cnn(df, var, distance = "euclidean")
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

- distance:

  A character string specifying the distance metric used for nearest
  neighbor calculations. One of `"euclidean"` (default), `"cosine"`,
  `"mahalanobis"`, `"manhattan"`, or `"chebyshev"`. `"euclidean"`,
  `"cosine"`, and `"mahalanobis"` use approximate nearest neighbors via
  the RANN package and scale well to large datasets. `"manhattan"` and
  `"chebyshev"` compute an exact O(n^2) distance matrix and may be slow
  for large datasets.

## Value

A data.frame or tibble, depending on type of `df`.

## Details

Condensed Nearest Neighbors (CNN) is an under-sampling method that
reduces the majority classes to a consistent subset: a subset that
classifies the original data correctly using a 1-nearest-neighbor rule.
It starts with a "store" containing all minority class observations and
one randomly chosen majority class observation. It then repeatedly scans
the remaining majority class observations and moves any that are
misclassified by a 1-nearest neighbor fit on the current store into the
store. This continues until a full pass adds no new observations. The
observations left outside the store are removed.

The smallest class is treated as the minority class and is always kept.
CNN tends to keep observations near the decision boundary while
discarding redundant interior observations. Because the seed observation
and the scan order are random, results depend on the random seed.

All columns used in this function must be numeric with no missing data.

## References

Hart, P. (1968). The condensed nearest neighbor rule. IEEE Transactions
on Information Theory, 14(3), 515-516.

## See also

[`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`oss()`](https://themis.tidymodels.org/dev/reference/oss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- cnn(circle_numeric, var = "class")

res <- cnn(circle_numeric, var = "class", distance = "manhattan")
```
