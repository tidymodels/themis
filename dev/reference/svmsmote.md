# SVM-SMOTE Algorithm

SVM-SMOTE generates new examples of the minority class near the decision
boundary, using the support vectors of a fitted SVM to decide where to
place synthetic examples.

## Usage

``` r
svmsmote(df, var, k = 5, over_ratio = 1, distance = "euclidean")
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
  (approximately) half as many rows as the majority level. See
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

SVM-SMOTE (Support Vector Machine SMOTE) works the same way as SMOTE,
except that instead of generating points around every point of the
minority class, it focuses generation near the decision boundary. A
support vector machine is fitted to the data and the support vectors
that belong to the minority class are used as the base points for
generating new examples.

For each minority support vector its nearest neighbors among all classes
are calculated. If all of the neighbors come from a different class the
support vector is labeled noise and is discarded. If more than half of
the neighbors come from a different class the support vector is labeled
"danger" and new points are interpolated between it and its
minority-class neighbors. The remaining support vectors are considered
to be in a safe region and new points are extrapolated away from their
minority-class neighbors.

SMOTE generates new examples of the minority class using nearest
neighbors of these cases. For each existing minority class example, new
examples are created by interpolating between the example and its
nearest neighbors. The number of nearest neighbors used is controlled by
the number of neighbors argument (`k` in
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
`neighbors` in
[`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)),
and the number of new examples generated is controlled by `over_ratio`.

All columns used in this function must be numeric with no missing data.

## References

Nguyen, H. M., Cooper, E. W., and Kamei, K. (2011). Borderline
over-sampling for imbalanced data classification. International Journal
of Knowledge Engineering and Soft Data Paradigms, 3(1), 4-21.

## See also

[`step_svmsmote()`](https://themis.tidymodels.org/dev/reference/step_svmsmote.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
[`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
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

res <- svmsmote(circle_numeric, var = "class")

res <- svmsmote(circle_numeric, var = "class", k = 10)

res <- svmsmote(circle_numeric, var = "class", over_ratio = 0.8)

res <- svmsmote(circle_numeric, var = "class", distance = "manhattan")
```
