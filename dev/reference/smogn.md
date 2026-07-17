# SMOGN Algorithm

SMOGN generates new examples for imbalanced regression problems. It
identifies rare regions of the continuous outcome using a relevance
function, over-samples those regions with a combination of SMOTE-style
interpolation and the introduction of Gaussian noise, and under-samples
the common regions.

## Usage

``` r
smogn(
  df,
  var,
  threshold = 0.5,
  relevance = NULL,
  neighbors = 5,
  perturbation = 0.02,
  distance = "euclidean"
)
```

## Arguments

- df:

  data.frame or tibble. Must have 1 numeric outcome variable and
  remaining numeric variables.

- var:

  Character, name of the numeric outcome variable.

- threshold:

  A number between 0 and 1. Outcome values with a relevance at or above
  this value are treated as rare and over-sampled. Defaults to `0.5`.

- relevance:

  A matrix of relevance control points, or `NULL` (default). When
  `NULL`, relevance is derived automatically from the boxplot extremes
  of the outcome. When supplied, the first column gives outcome values
  and the second column their relevance in `[0, 1]`.

- neighbors:

  An integer. Number of nearest neighbor that are used to generate the
  new examples of the rare values.

- perturbation:

  A number. The magnitude of the Gaussian noise added when generating
  synthetic examples in unsafe regions. Defaults to `0.02`.

- distance:

  A character string specifying the distance metric used for nearest
  neighbor calculations. One of `"euclidean"` (default), `"cosine"`,
  `"mahalanobis"`, `"manhattan"`, or `"chebyshev"`.

## Value

A data.frame or tibble, depending on type of `df`.

## Details

SMOGN is a pre-processing approach for imbalanced regression. A
relevance function assigns each outcome value a relevance score, and
values with a relevance at or above `threshold` are treated as rare. The
data is split into contiguous bins of rare and common outcome values.
Common bins are under-sampled and rare bins are over-sampled toward a
balanced size. New rare examples are generated either by interpolating
between an example and a nearby neighbor (when they are close enough to
be considered safe) or by perturbing the example with Gaussian noise
(when they are not), where the amount of noise is controlled by
`perturbation`.

By default relevance is derived automatically from the boxplot extremes
of the outcome, giving the median a relevance of 0 and the extreme
values a relevance of 1. A matrix of relevance control points can
instead be supplied through `relevance`, with the first column giving
outcome values and the second column their relevance.

All columns used in this function must be numeric with no missing data.

## References

Branco, P., Torgo, L., and Ribeiro, R. P. (2017). SMOGN: a
pre-processing approach for imbalanced regression. Proceedings of
Machine Learning Research, 74:36-50.

## See also

[`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
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
[`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
[`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md),
[`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y")]

res <- smogn(circle_numeric, var = "x")

res <- smogn(circle_numeric, var = "x", neighbors = 10)
```
