# Package index

## Over-sampling

**Over-sampling** is the act of synthetically generating observations
for the minority classes. This is done either by random sampling or
using more advanced methods.

- [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  : Up-Sample a Data Set Based on a Factor Variable
- [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)
  : Apply SMOTE Algorithm
- [`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
  : Apply SMOGN Algorithm
- [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  : Apply SMOTENC algorithm
- [`step_smoten()`](https://themis.tidymodels.org/dev/reference/step_smoten.md)
  : Apply SMOTEN algorithm
- [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
  : Apply borderline-SMOTE Algorithm
- [`step_svmsmote()`](https://themis.tidymodels.org/dev/reference/step_svmsmote.md)
  : Apply SVM-SMOTE Algorithm
- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
  : Apply Adaptive Synthetic Algorithm
- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  : Apply ROSE Algorithm

## Under-sampling

**Under-sampling** is the act of removing observations from the majority
classes.

- [`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md)
  : Condensed Nearest Neighbors
- [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  : Down-Sample a Data Set Based on a Factor Variable
- [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  : Edited Nearest Neighbors
- [`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md)
  : Remove hard to classify points
- [`step_ncl()`](https://themis.tidymodels.org/dev/reference/step_ncl.md)
  : Neighborhood Cleaning Rule
- [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
  : Remove Points Near Other Classes
- [`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md)
  : One-Sided Selection
- [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  : Remove Tomek’s Links

## Methods

Some of the methods implemented in this package as steps are also
available as their own functions.

- [`smote()`](https://themis.tidymodels.org/dev/reference/smote.md) :
  SMOTE Algorithm
- [`smogn()`](https://themis.tidymodels.org/dev/reference/smogn.md) :
  SMOGN Algorithm
- [`smotenc()`](https://themis.tidymodels.org/dev/reference/smotenc.md)
  : SMOTENC Algorithm
- [`smoten()`](https://themis.tidymodels.org/dev/reference/smoten.md) :
  SMOTEN Algorithm
- [`bsmote()`](https://themis.tidymodels.org/dev/reference/bsmote.md) :
  borderline-SMOTE Algorithm
- [`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md)
  : SVM-SMOTE Algorithm
- [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md) :
  Adaptive Synthetic Algorithm
- [`rose()`](https://themis.tidymodels.org/dev/reference/rose.md) : ROSE
  Algorithm
- [`cnn()`](https://themis.tidymodels.org/dev/reference/cnn.md) :
  Condensed Nearest Neighbors
- [`enn()`](https://themis.tidymodels.org/dev/reference/enn.md) : Edited
  Nearest Neighbors
- [`instance_hardness()`](https://themis.tidymodels.org/dev/reference/instance_hardness.md)
  : Remove hard to classify points
- [`ncl()`](https://themis.tidymodels.org/dev/reference/ncl.md) :
  Neighborhood Cleaning Rule
- [`nearmiss()`](https://themis.tidymodels.org/dev/reference/nearmiss.md)
  : Remove Points Near Other Classes
- [`oss()`](https://themis.tidymodels.org/dev/reference/oss.md) :
  One-Sided Selection
- [`tomek()`](https://themis.tidymodels.org/dev/reference/tomek.md) :
  Remove Tomek's links

## Datasets

- [`circle_example`](https://themis.tidymodels.org/dev/reference/circle_example.md)
  : Synthetic Dataset With a Circle
