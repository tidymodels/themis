# ClusterCentroids Algorithm

Under-samples the majority classes by replacing them with cluster
representatives found with k-means.

## Usage

``` r
cluster_centroids(df, var, under_ratio = 1, voting = "soft")
```

## Arguments

- df:

  data.frame or tibble. Must have 1 factor variable and remaining
  numeric variables.

- var:

  Character, name of variable containing factor variable.

- under_ratio:

  A numeric value for the ratio of the majority-to-minority frequencies.
  The default value (1) means that all other levels are sampled down to
  have the same frequency as the least occurring level. A value of 2
  would mean that the majority levels will have (at most)
  (approximately) twice as many rows than the minority level.

  A named numeric vector can be used instead to give different levels
  different targets, for example `c(a = 2, b = 3)`. The names must be
  levels of the outcome and the values are ratios of the minority level,
  exactly as in the single-number case. Levels that are not named are
  left untouched, as are rows with a missing outcome. Because a vector
  of targets is not a single value, supplying one means this argument
  can no longer be tuned. See `vignette("ratio", package = "themis")`
  for more details.

- voting:

  A character string. Either `"soft"` (the default) to use the cluster
  centroids themselves, or `"hard"` to use the observation closest to
  each centroid.

## Value

A data.frame or tibble, depending on type of `df`.

## Details

Each class larger than the target count is summarized by running k-means
on the observations of that class, using as many clusters as the target
count. The class is then replaced by one representative per cluster:

- `voting = "soft"`:

  the cluster centroids themselves are used, so the returned
  observations are synthetic points that need not appear in the input.

- `voting = "hard"`:

  the observation closest to each centroid is used, so all returned
  observations are real rows. The representative is always picked from
  the class being under-sampled.

This makes it the one *prototype generation* under-sampler in this
package. The other under-sampling methods perform *prototype selection*,
keeping a subset of the original rows.

Because two clusters can share the same closest observation,
`voting = "hard"` can return slightly fewer observations than the target
count.

All columns used in this function must be numeric with no missing data.

## See also

[`step_cluster_centroids()`](https://themis.tidymodels.org/reference/step_cluster_centroids.md)
for step function of this method

Other Direct Implementations:
[`adasyn()`](https://themis.tidymodels.org/reference/adasyn.md),
[`bsmote()`](https://themis.tidymodels.org/reference/bsmote.md),
[`cnn()`](https://themis.tidymodels.org/reference/cnn.md),
[`enn()`](https://themis.tidymodels.org/reference/enn.md),
[`instance_hardness()`](https://themis.tidymodels.org/reference/instance_hardness.md),
[`kmeans_smote()`](https://themis.tidymodels.org/reference/kmeans_smote.md),
[`ncl()`](https://themis.tidymodels.org/reference/ncl.md),
[`nearmiss()`](https://themis.tidymodels.org/reference/nearmiss.md),
[`oss()`](https://themis.tidymodels.org/reference/oss.md),
[`rose()`](https://themis.tidymodels.org/reference/rose.md),
[`smogn()`](https://themis.tidymodels.org/reference/smogn.md),
[`smote()`](https://themis.tidymodels.org/reference/smote.md),
[`smoten()`](https://themis.tidymodels.org/reference/smoten.md),
[`smotenc()`](https://themis.tidymodels.org/reference/smotenc.md),
[`svmsmote()`](https://themis.tidymodels.org/reference/svmsmote.md),
[`tomek()`](https://themis.tidymodels.org/reference/tomek.md)

## Examples

``` r
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- cluster_centroids(circle_numeric, var = "class")

res <- cluster_centroids(circle_numeric, var = "class", voting = "hard")

res <- cluster_centroids(circle_numeric, var = "class", under_ratio = 1.5)
```
