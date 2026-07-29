# Methods overview

themis provides a large family of steps for dealing with imbalanced
data. This article organizes them into a taxonomy, borrowed from
[imbalanced-learn](https://imbalanced-learn.org/), so you can see how
the methods relate to each other and choose between them. Every method
is a recipe step, and most also have a direct-implementation function of
the same name without the `step_` prefix.

``` r

library(recipes)
library(themis)
```

## Over-sampling

Over-sampling grows the minority classes, either by replicating existing
observations or by synthesizing new ones.

- **Random replication.**
  [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  samples minority observations with replacement. It adds no new
  information but is fast and model-agnostic.
- **SMOTE family.** These synthesize new observations by interpolating
  between a minority observation and its nearest neighbors.
  - [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)
    is the original SMOTE for numeric predictors.
  - [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
    (borderline-SMOTE) synthesizes only near the class boundary, where
    errors are most likely.
  - [`step_svmsmote()`](https://themis.tidymodels.org/dev/reference/step_svmsmote.md)
    uses a support vector machine to locate the boundary before
    synthesizing.
  - [`step_kmeans_smote()`](https://themis.tidymodels.org/dev/reference/step_kmeans_smote.md)
    clusters the predictor space first and synthesizes only inside the
    clusters where the minority class is dominant, giving sparser
    clusters more of the new observations.
  - [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
    handles a mix of numeric and nominal predictors.
  - [`step_smoten()`](https://themis.tidymodels.org/dev/reference/step_smoten.md)
    handles data sets where every predictor is nominal.
- **Adaptive.**
  [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
  is like SMOTE but generates more synthetic points for the minority
  observations that are harder to learn (those surrounded by majority
  neighbors).
- **Smoothed bootstrap.**
  [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  generates a fully synthetic data set by drawing from smoothed kernels
  around the observations.

## Under-sampling

Under-sampling shrinks the majority classes. Most under-samplers in
themis perform *prototype selection*, keeping a subset of the real rows.
One,
[`step_cluster_centroids()`](https://themis.tidymodels.org/dev/reference/step_cluster_centroids.md),
performs *prototype generation*, creating new representative rows
instead.

- **Random.**
  [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  drops majority observations at random.
- **Distance / near-boundary.** These target observations by their
  position relative to the class boundary.
  - [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
    keeps the majority observations closest to the minority class.
  - [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
    removes the majority member of each Tomek link (a mutually-nearest
    pair from different classes).
  - [`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md)
    (condensed nearest neighbors) keeps a subset that still classifies
    the data with a 1-nearest-neighbor rule.
  - [`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md)
    (one-sided selection) combines condensed nearest neighbors with
    Tomek links.
- **Neighborhood cleaning.** These remove observations that disagree
  with their neighborhood.
  - [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
    (edited nearest neighbors) removes observations whose class differs
    from the majority of their neighbors. With `times` or `all_k` it
    becomes RENN or AllKNN.
  - [`step_ncl()`](https://themis.tidymodels.org/dev/reference/step_ncl.md)
    (neighborhood cleaning rule) is a variant focused on cleaning the
    neighborhoods of minority observations.
- **Hardness.**
  [`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md)
  removes the majority observations that are hardest to classify,
  measured by how often their neighbors disagree.
- **Prototype generation.**
  [`step_cluster_centroids()`](https://themis.tidymodels.org/dev/reference/step_cluster_centroids.md)
  summarizes each majority class with k-means cluster representatives:
  the centroids themselves with `voting = "soft"`, or the observation
  nearest each centroid with `voting = "hard"`.

## Combination

imbalanced-learn ships combination samplers (`SMOTEENN`, `SMOTETomek`)
that over-sample and then clean the result. scikit-learn needs these as
single objects because a pipeline cannot chain two samplers around one
estimator. recipes has no such limitation, so the equivalents are
ordinary step compositions.

``` r

# SMOTEENN: over-sample, then clean with edited nearest neighbors
recipe(class ~ ., data = train) |>
  step_smote(class) |>
  step_enn(class)

# SMOTETomek: over-sample, then remove Tomek links
recipe(class ~ ., data = train) |>
  step_smote(class) |>
  step_tomek(class)
```

Two things are worth knowing:

- **Order matters.** Over-sample first, then clean, so the cleaning step
  can remove the ambiguous synthetic points SMOTE tends to create near
  the boundary.
- **[`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  cleans every class.** It flags any observation, including freshly
  synthesized minority points, whose class differs from its
  neighborhood. This matches imbalanced-learn’s `SMOTEENN`, which
  configures its ENN stage with `sampling_strategy = "all"`. Standalone
  imbalanced-learn `EditedNearestNeighbours` instead defaults to
  cleaning “not minority”, so themis’s
  [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  is already oriented toward this combined use.

## Regression

Imbalance is not limited to classification.
[`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
resamples an imbalanced *numeric* outcome, over-sampling rare regions of
the response and under-sampling common ones, so themis applies to
imbalanced regression as well.

## Choosing a method

- Start with
  [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  or
  [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  as fast baselines.
- Reach for the SMOTE family when synthesizing plausible minority
  observations helps, and pick the member that matches your predictor
  types
  ([`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  /
  [`step_smoten()`](https://themis.tidymodels.org/dev/reference/step_smoten.md)
  for nominal data).
- Use a cleaning under-sampler
  ([`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md),
  [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md),
  [`step_ncl()`](https://themis.tidymodels.org/dev/reference/step_ncl.md))
  when the majority class is noisy or overlaps the minority class, on
  its own or composed after SMOTE.
- Use
  [`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
  for imbalanced regression.
