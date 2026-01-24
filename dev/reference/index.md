# Package index

## Over-sampling

**Over-sampling** is the act of synthetically generating observations
for the minority classes. This is done either by random sampling or
using more advanced methods.

- [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  : Up-Sample a Data Set Based on a Factor Variable
- [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)
  : Apply SMOTE Algorithm
- [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  : Apply SMOTENC algorithm
- [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
  : Apply borderline-SMOTE Algorithm
- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
  : Apply Adaptive Synthetic Algorithm
- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  : Apply ROSE Algorithm

## Under-sampling

**Under-sampling** is the act of removing observations from the majority
classes.

- [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  : Down-Sample a Data Set Based on a Factor Variable
- [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
  : Remove Points Near Other Classes
- [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  : Remove Tomek’s Links

## Methods

Some of the methods implemented in this package as steps are also
available as their own functions.

- [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md) :
  SMOTE Algorithm
- [`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md)
  : SMOTENC Algorithm
- [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md) :
  borderline-SMOTE Algorithm
- [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md) :
  Adaptive Synthetic Algorithm
- [`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md)
  : Remove Points Near Other Classes
- [`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md) :
  Remove Tomek's links

## Datasets

- [`circle_example`](https://themis.tidymodels.org/dev/reference/circle_example.md)
  : Synthetic Dataset With a Circle
