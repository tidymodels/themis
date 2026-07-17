# Remove Points Near Other Classes

Generates synthetic positive instances using nearmiss algorithm.

## Usage

``` r
nearmiss(df, var, k = 5, under_ratio = 1, distance = "euclidean")
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

This implements the NearMiss-1 algorithm. It retains the points from the
majority class which have the smallest mean distance to the nearest
points in the minority class.

All columns used in this function must be numeric with no missing data.

## References

Inderjeet Mani and I Zhang. knn approach to unbalanced data
distributions: a case study involving information extraction. In
Proceedings of workshop on learning from imbalanced datasets, 2003.

## See also

[`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
[`enn()`](https://themis.tidymodels.org/dev/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md),
[`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md),
[`oss()`](https://themis.tidymodels.org/dev/reference/oss.md),
[`rose()`](https://themis.tidymodels.org/dev/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- nearmiss(circle_numeric, var = "class")

res <- nearmiss(circle_numeric, var = "class", k = 10)

res <- nearmiss(circle_numeric, var = "class", under_ratio = 1.5)

res <- nearmiss(circle_numeric, var = "class", distance = "manhattan")
```
