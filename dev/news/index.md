# Changelog

## themis (development version)

- All `step_*()` functions now correctly handle 0 and 1 row inputs in
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)
  ([\#160](https://github.com/tidymodels/themis/issues/160)).

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
