# Remove hard to classify points

Under-samples the majority classes by removing the points that are
hardest to classify.

## Usage

``` r
instance_hardness(df, var, k = 5, under_ratio = 1, distance = "euclidean")
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

- k:

  An integer. Number of nearest neighbors used to estimate the instance
  hardness of each observation.

- under_ratio:

  A numeric value for the ratio of the majority-to-minority frequencies.
  The default value (1) means that all other levels are sampled down to
  have the same frequency as the least occurring level. A value of 2
  would mean that the majority levels will have (at most)
  (approximately) twice as many rows than the minority level. See
  `vignette("ratio", package = "themis")` for more details.

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

The instance hardness of each observation is estimated using the
k-Disagreeing Neighbors measure: the proportion of the nearest neighbors
that belong to a different class. Observations that are surrounded by
points of a different class are considered hard to classify. For each
majority class, the hardest observations are removed until the desired
`under_ratio` is reached.

All columns used in this function must be numeric with no missing data.

## References

Smith, M. R., Martinez, T., & Giraud-Carrier, C. (2014). An instance
level analysis of data complexity. Machine learning, 95(2), 225-256.

## See also

[`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- instance_hardness(circle_numeric, var = "class")

res <- instance_hardness(circle_numeric, var = "class", k = 10)

res <- instance_hardness(circle_numeric, var = "class", under_ratio = 1.5)

res <- instance_hardness(circle_numeric, var = "class", distance = "manhattan")
```
