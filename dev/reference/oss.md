# One-Sided Selection

Under-samples the majority classes by combining Condensed Nearest
Neighbors and Tomek's links, first reducing redundant majority class
observations and then removing majority class observations that form
Tomek links with minority class observations.

## Usage

``` r
oss(df, var, distance = "euclidean")
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

One-Sided Selection (OSS) is an under-sampling method that combines two
cleaning techniques. It first applies Condensed Nearest Neighbors (CNN)
to reduce the majority classes to a consistent subset that correctly
classifies the data using a 1-nearest-neighbor rule, discarding
redundant interior observations. It then applies Tomek's links to the
remaining observations, removing the majority class observations that
form Tomek links with minority class observations, cleaning the decision
boundary.

The smallest class is treated as the minority class and is always kept.
Because the CNN step relies on a random seed observation and a random
scan order, results depend on the random seed.

All columns used in this function must be numeric with no missing data.

## References

Kubat, M., & Matwin, S. (1997). Addressing the curse of imbalanced
training sets: one-sided selection. In ICML (Vol. 97, pp. 179-186).

## See also

[`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- oss(circle_numeric, var = "class")

res <- oss(circle_numeric, var = "class", distance = "manhattan")
```
