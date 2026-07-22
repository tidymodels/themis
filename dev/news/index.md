# Changelog

## themis (development version)

- All sampling steps now handle an unused (zero-count) factor level in
  the outcome gracefully, dropping it with a warning before computing
  sampling targets instead of deleting all rows or erroring
  ([\#238](https://github.com/tidymodels/themis/issues/238)).

- [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md),
  [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md),
  [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md),
  [`step_svmsmote()`](https://themis.tidymodels.org/dev/reference/step_svmsmote.md),
  [`step_smoten()`](https://themis.tidymodels.org/dev/reference/step_smoten.md),
  and
  [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  (and their direct-implementation counterparts) now round the
  fractional oversampling target instead of truncating it, so a
  fractional `over_ratio` lands on the nearest integer count
  ([\#248](https://github.com/tidymodels/themis/issues/248)).

- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
  (and its direct-implementation counterpart
  [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md))
  now weights minority observations by their exact majority-neighbor
  count. An off-by-one subtraction previously undercounted majority
  neighbors, zeroing out the weight of border points with a single
  majority neighbor and biasing sampling away from the class boundary
  ([\#239](https://github.com/tidymodels/themis/issues/239)).

- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
  (and its direct-implementation counterpart
  [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md))
  no longer errors with a cryptic message when a minority class is well
  separated from the majority classes; it now falls back to uniform
  sampling and checks the minority class size before sampling
  ([\#240](https://github.com/tidymodels/themis/issues/240)).

- [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
  (and its direct-implementation counterpart
  [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md))
  now selects the correct “danger” observations on the class border. The
  danger criterion had inverted the roles of minority and majority
  neighbors, causing it to oversample safe interior points instead of
  borderline ones
  ([\#235](https://github.com/tidymodels/themis/issues/235)).

- [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
  (and its direct-implementation counterpart
  [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md))
  with `all_neighbors = TRUE` now seeds synthetic points only from
  minority-class danger observations and takes a reduced step toward
  majority-class neighbors, matching borderline-SMOTE2. Previously it
  could seed from border-adjacent majority rows, generating
  minority-labeled points around majority centers
  ([\#242](https://github.com/tidymodels/themis/issues/242)).

- [`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md)
  (and its direct-implementation counterpart
  [`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md)) was
  added. It under-samples the majority classes using Condensed Nearest
  Neighbors, keeping only a consistent subset of observations that
  correctly classifies the data using a 1-nearest-neighbor rule
  ([\#113](https://github.com/tidymodels/themis/issues/113)).

- [`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md),
  [`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md),
  and
  [`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
  (and their direct-implementation counterparts
  [`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md),
  [`oss()`](https://themis.tidymodels.org/dev/reference/oss.md), and
  [`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md)) now
  sample correctly when only one candidate remains. A length-1 vector
  passed to [`sample()`](https://rdrr.io/r/base/sample.html) was
  interpreted as a count and sampled from `1:n`, which could select the
  wrong observations
  ([\#245](https://github.com/tidymodels/themis/issues/245)).

- [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  (and its direct-implementation counterpart
  [`enn()`](https://themis.tidymodels.org/dev/reference/enn.md)) was
  added. It cleans the data using the Edited Nearest Neighbors rule,
  removing observations whose class differs from the majority of their
  nearest neighbors
  ([\#115](https://github.com/tidymodels/themis/issues/115)).

- [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  (and its direct-implementation counterpart
  [`enn()`](https://themis.tidymodels.org/dev/reference/enn.md)) gain a
  `times` argument to apply the cleaning repeatedly, stopping early on
  convergence, which corresponds to Repeated Edited Nearest Neighbors
  (RENN) ([\#173](https://github.com/tidymodels/themis/issues/173)).

- [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  (and its direct-implementation counterpart
  [`enn()`](https://themis.tidymodels.org/dev/reference/enn.md)) gain an
  `all_k` argument to apply the cleaning with an increasing number of
  neighbors, from 1 up to `neighbors`, which corresponds to All
  k-Nearest Neighbors (AllKNN)
  ([\#174](https://github.com/tidymodels/themis/issues/174)).

- [`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md)
  (and its direct-implementation counterpart
  [`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md))
  was added. It under-samples the majority classes by removing the
  observations that are hardest to classify, estimated using the
  k-Disagreeing Neighbors measure
  ([\#172](https://github.com/tidymodels/themis/issues/172)).

- [`step_ncl()`](https://themis.tidymodels.org/dev/reference/step_ncl.md)
  (and its direct-implementation counterpart
  [`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md)) was
  added. It cleans the data using the Neighborhood Cleaning Rule,
  removing majority class observations that are noisy or that pollute
  the neighborhood of minority class observations
  ([\#116](https://github.com/tidymodels/themis/issues/116)).

- [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
  (and its direct-implementation counterpart
  [`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md))
  now keeps the majority observations that are genuinely closest to the
  minority class, rather than selecting rows by their position in the
  data ([\#236](https://github.com/tidymodels/themis/issues/236)).

- [`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md)
  (and its direct-implementation counterpart
  [`oss()`](https://themis.tidymodels.org/dev/reference/oss.md)) was
  added. It under-samples the majority classes using One-Sided
  Selection, combining Condensed Nearest Neighbors to reduce redundant
  majority class observations with Tomek’s links to remove majority
  class observations on the decision boundary
  ([\#114](https://github.com/tidymodels/themis/issues/114)).

- [`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
  (and its direct-implementation counterpart
  [`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md)) was
  added. It over-samples rare regions of a numeric outcome for
  imbalanced regression using a combination of SMOTE-style interpolation
  and Gaussian noise, while under-sampling common regions
  ([\#49](https://github.com/tidymodels/themis/issues/49)).

- [`step_smoten()`](https://themis.tidymodels.org/dev/reference/step_smoten.md)
  (and its direct-implementation counterpart
  [`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md))
  was added. It over-samples the minority classes for data sets where
  all predictors are categorical, using the Value Difference Metric to
  find nearest neighbors and majority voting to generate new examples
  ([\#54](https://github.com/tidymodels/themis/issues/54)).

- [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  (and its direct-implementation counterpart
  [`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md))
  now sets each synthetic sample’s nominal features to the majority vote
  across the seed’s k nearest neighbors, matching the SMOTENC algorithm,
  rather than voting over the randomly chosen interpolation partners
  ([\#241](https://github.com/tidymodels/themis/issues/241)).

- [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  (and its direct-implementation counterpart
  [`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md)) now
  removes only the majority-class member of each Tomek link, retaining
  the minority-class member, matching the documented behavior.
  Previously it removed both members of the pair
  ([\#262](https://github.com/tidymodels/themis/issues/262)).

- [`step_svmsmote()`](https://themis.tidymodels.org/dev/reference/step_svmsmote.md)
  (and its direct-implementation counterpart
  [`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md))
  was added. It over-samples the minority classes near the decision
  boundary by fitting a support vector machine and generating new
  examples around the minority class support vectors, interpolating in
  dense regions and extrapolating in sparse ones
  ([\#170](https://github.com/tidymodels/themis/issues/170)).

- [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  now names itself, rather than
  [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md),
  in the deprecation message shown when the defunct `ratio` argument is
  supplied ([\#252](https://github.com/tidymodels/themis/issues/252)).

- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md),
  [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md),
  [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md),
  [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md),
  and
  [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  (and their direct-implementation counterparts
  [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
  [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
  [`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
  [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md), and
  [`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md))
  gain a `distance` argument to control which distance metric is used
  for nearest neighbor calculations. Supported metrics are `"euclidean"`
  (default), `"cosine"`, `"mahalanobis"`, `"manhattan"`, and
  `"chebyshev"`
  ([\#171](https://github.com/tidymodels/themis/issues/171)).

- Added a new article explaining how `over_ratio` and `under_ratio` work
  ([\#141](https://github.com/tidymodels/themis/issues/141)).

- All upsampling steps gain an `indicator_column` argument. When set, a
  logical column is added to the baked data marking rows added by the
  step (`TRUE`) vs rows from the original data (`FALSE`). For
  [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md),
  all rows are `TRUE` since ROSE generates a fully synthetic dataset
  ([\#58](https://github.com/tidymodels/themis/issues/58)).

- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  now validates predictor types during
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html), giving
  a clear error for unsupported types consistent with the other sampling
  steps instead of relying on
  [`ROSE::ROSE()`](https://rdrr.io/pkg/ROSE/man/ROSE.html) to fail
  downstream ([\#265](https://github.com/tidymodels/themis/issues/265)).

- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  and [`rose()`](https://themis.tidymodels.org/dev/reference/rose.md)
  now have improved documentation for `minority_prop`, clarifying that
  it controls the proportion of synthetic observations from the minority
  class, and how it differs from `over_ratio`
  ([\#144](https://github.com/tidymodels/themis/issues/144)).

- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  (and its direct-implementation counterpart
  [`rose()`](https://themis.tidymodels.org/dev/reference/rose.md)) now
  validate that `minority_prop` is at most 1, since it is a proportion
  ([\#269](https://github.com/tidymodels/themis/issues/269)).

- Added standalone
  [`rose()`](https://themis.tidymodels.org/dev/reference/rose.md)
  function as a thin wrapper around
  [`ROSE::ROSE()`](https://rdrr.io/pkg/ROSE/man/ROSE.html), making it
  consistent with the other algorithms in the package that expose a
  direct implementation alongside their recipe step
  ([\#195](https://github.com/tidymodels/themis/issues/195)).

- [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
  and
  [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  gain a `distance_with` argument to control which variables are used
  for distance calculations. This allows the steps to be used when
  non-numeric predictor variables are present in the data
  ([\#166](https://github.com/tidymodels/themis/issues/166)).

- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md),
  [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md),
  [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md),
  [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md),
  and
  [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  now document the minimum number of observations needed to perform the
  algorithm ([\#104](https://github.com/tidymodels/themis/issues/104)).

- All `step_*()` functions now correctly handle 0 and 1 row inputs in
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)
  ([\#160](https://github.com/tidymodels/themis/issues/160)).

- [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
  [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
  [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
  [`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md),
  [`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md),
  and
  [`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md)
  now return a proper factor outcome when called with a character `var`,
  instead of an all-`NA`, zero-level factor
  ([\#261](https://github.com/tidymodels/themis/issues/261)).

- [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
  [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md),
  [`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md),
  [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md), and
  [`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md) now
  correctly attribute errors from non-numeric columns to the user-facing
  function ([\#181](https://github.com/tidymodels/themis/issues/181)).

- [`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md)
  now only suppresses the specific benign warning from
  [`gower::gower_topn()`](https://rdrr.io/pkg/gower/man/gower_topn.html)
  about variables with zero range, rather than all warnings
  ([\#182](https://github.com/tidymodels/themis/issues/182)).

- [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md)
  now correctly passes the `all_neighbors` argument to the underlying
  implementation
  ([\#176](https://github.com/tidymodels/themis/issues/176)).

- [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
  now works correctly when there is only a single predictor
  ([\#151](https://github.com/tidymodels/themis/issues/151)).

- [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  and
  [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  now correctly handle `NA` values in the outcome variable instead of
  erroring ([\#177](https://github.com/tidymodels/themis/issues/177)).

## themis 1.0.3

CRAN release: 2025-01-22

### Improvements

- Calling `?tidy.step_*()` now sends you to the documentation for
  `step_*()` where the outcome is documented.
  ([\#142](https://github.com/tidymodels/themis/issues/142))

- Documentation now correctly specifies majority-to-minority and
  minority-to-majority.
  ([\#143](https://github.com/tidymodels/themis/issues/143),
  [\#110](https://github.com/tidymodels/themis/issues/110))

- Documentation for tidy methods for all steps has been improved to
  describe the return value more accurately.
  ([\#148](https://github.com/tidymodels/themis/issues/148))

- All messages, warnings and errors has been translated to use {cli}
  package ([\#153](https://github.com/tidymodels/themis/issues/153),
  [\#155](https://github.com/tidymodels/themis/issues/155)).

## themis 1.0.2

CRAN release: 2023-08-14

### Improvements

- Many internal changes to improve consistency and slight speed
  increases.

## themis 1.0.1

CRAN release: 2023-04-14

### Improvements

- Fixed bug where some upsampling functions would error if no upsampling
  was needed. ([\#119](https://github.com/tidymodels/themis/issues/119))

- Steps with tunable arguments now have those arguments listed in the
  documentation.

## themis 1.0.0

CRAN release: 2022-07-02

- Added case weights support for
  [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  and
  [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)

## themis 0.2.2

CRAN release: 2022-05-11

- [`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md) has
  been added, rewritten to apply to multiple classes, removing the need
  for the unbalanced package, which has been removed as a dependency.

## themis 0.2.1

CRAN release: 2022-04-13

- A bug was fixed in
  [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  and
  [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  that made the steps unable to be tuned.
  ([\#90](https://github.com/tidymodels/themis/issues/90))

## themis 0.2.0

CRAN release: 2022-03-30

### New steps

- [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  have been added to implement SMOTENC which can handle categorical as
  well as numerical values. Thanks to
  [@RobertGregg](https://github.com/RobertGregg)
  ([\#82](https://github.com/tidymodels/themis/issues/82))

### Improvements and Other Changes

- export
  [`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md)
  functions to users.
- Update examples to no longer use `iris` or `okc` data sets.
- All recipe steps now officially support empty selections to be more
  aligned with dplyr and other packages that use tidyselect
  ([\#55](https://github.com/tidymodels/themis/issues/55))

### Bug fixes

- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  now correctly allows you to use characters variables.
  ([\#26](https://github.com/tidymodels/themis/issues/26))
- [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  now ignore non-predictor variables when appropriate.
  ([\#51](https://github.com/tidymodels/themis/issues/51))
- Fix bug where wrong ordering of columns caused error in
  [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md).
  ([\#76](https://github.com/tidymodels/themis/issues/76))

## themis 0.1.4

CRAN release: 2021-06-12

- export
  [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md),
  [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md),
  and
  [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md)
  functions to users.

## themis 0.1.3

CRAN release: 2020-11-12

- Steps that use nearest neighbors gives cleaner errors.

## themis 0.1.2

CRAN release: 2020-08-14

- tuneable steps now properly work with tune package.
- Steps now Retain original factor level ordering.
  ([\#22](https://github.com/tidymodels/themis/issues/22))
- Oversampling steps now ignore non-predictor variables when
  appropriate. ([\#20](https://github.com/tidymodels/themis/issues/20))

## themis 0.1.1

CRAN release: 2020-05-17

- [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)
  now work regardless of order of classes. Thanks to
  [@sebastien-foulle](https://github.com/sebastien-foulle) for point it
  out [\#14](https://github.com/tidymodels/themis/issues/14).

## themis 0.1.0

CRAN release: 2020-01-13

- Added a `NEWS.md` file to track changes to the package.
