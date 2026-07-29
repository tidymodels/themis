# Package index

## Over-sampling

**Over-sampling** is the act of synthetically generating observations
for the minority classes, either by random replication or by more
advanced methods that synthesize new observations.

### Random replication

- [`step_upsample()`](https://themis.tidymodels.org/dev/reference/step_upsample.md)
  : Up-Sample a Data Set Based on a Factor Variable

### SMOTE family

- [`step_smote()`](https://themis.tidymodels.org/dev/reference/step_smote.md)
  : Apply SMOTE Algorithm
- [`step_bsmote()`](https://themis.tidymodels.org/dev/reference/step_bsmote.md)
  : Apply Borderline-SMOTE Algorithm
- [`step_svmsmote()`](https://themis.tidymodels.org/dev/reference/step_svmsmote.md)
  : Apply SVM-SMOTE Algorithm
- [`step_kmeans_smote()`](https://themis.tidymodels.org/dev/reference/step_kmeans_smote.md)
  : Apply KMeans-SMOTE Algorithm
- [`step_smotenc()`](https://themis.tidymodels.org/dev/reference/step_smotenc.md)
  : Apply SMOTENC Algorithm
- [`step_smoten()`](https://themis.tidymodels.org/dev/reference/step_smoten.md)
  : Apply SMOTEN Algorithm

### Adaptive

- [`step_adasyn()`](https://themis.tidymodels.org/dev/reference/step_adasyn.md)
  : Apply Adaptive Synthetic Algorithm

### Smoothed bootstrap

- [`step_rose()`](https://themis.tidymodels.org/dev/reference/step_rose.md)
  : Apply ROSE Algorithm

## Under-sampling

**Under-sampling** is the act of removing observations from the majority
classes. Most under-samplers here use *prototype selection*, keeping a
subset of the real rows.
[`step_cluster_centroids()`](https://themis.tidymodels.org/dev/reference/step_cluster_centroids.md)
instead uses *prototype generation*, creating new representative rows.

### Random

- [`step_downsample()`](https://themis.tidymodels.org/dev/reference/step_downsample.md)
  : Down-Sample a Data Set Based on a Factor Variable

### Distance / near-boundary

- [`step_nearmiss()`](https://themis.tidymodels.org/dev/reference/step_nearmiss.md)
  : Remove Points Near Other Classes
- [`step_tomek()`](https://themis.tidymodels.org/dev/reference/step_tomek.md)
  : Remove Tomek's Links
- [`step_cnn()`](https://themis.tidymodels.org/dev/reference/step_cnn.md)
  : Condensed Nearest Neighbors
- [`step_oss()`](https://themis.tidymodels.org/dev/reference/step_oss.md)
  : One-Sided Selection

### Neighborhood cleaning

- [`step_enn()`](https://themis.tidymodels.org/dev/reference/step_enn.md)
  : Edited Nearest Neighbors
- [`step_ncl()`](https://themis.tidymodels.org/dev/reference/step_ncl.md)
  : Neighborhood Cleaning Rule

### Hardness

- [`step_instance_hardness()`](https://themis.tidymodels.org/dev/reference/step_instance_hardness.md)
  : Remove hard to classify points

### Prototype generation

- [`step_cluster_centroids()`](https://themis.tidymodels.org/dev/reference/step_cluster_centroids.md)
  : Under-Sampling by Cluster Centroids

## Regression

Resampling an imbalanced numeric outcome rather than a class.

- [`step_smogn()`](https://themis.tidymodels.org/dev/reference/step_smogn.md)
  : Apply SMOGN Algorithm

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
  Borderline-SMOTE Algorithm
- [`svmsmote()`](https://themis.tidymodels.org/dev/reference/svmsmote.md)
  : SVM-SMOTE Algorithm
- [`kmeans_smote()`](https://themis.tidymodels.org/dev/reference/kmeans_smote.md)
  : KMeans-SMOTE Algorithm
- [`adasyn()`](https://themis.tidymodels.org/dev/reference/adasyn.md) :
  Adaptive Synthetic Algorithm
- [`rose()`](https://themis.tidymodels.org/dev/reference/rose.md) : ROSE
  Algorithm
- [`cluster_centroids()`](https://themis.tidymodels.org/dev/reference/cluster_centroids.md)
  : ClusterCentroids Algorithm
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
  Remove Tomek's Links

## Datasets

- [`circle_example`](https://themis.tidymodels.org/dev/reference/circle_example.md)
  : Synthetic Dataset With a Circle

## Developer tools

- [`required_pkgs(`*`<step_adasyn>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_bsmote>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_cluster_centroids>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_cnn>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_downsample>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_enn>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_instance_hardness>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_kmeans_smote>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_ncl>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_nearmiss>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_oss>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_rose>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_smogn>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_smote>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_smoten>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_smotenc>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_svmsmote>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_tomek>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  [`required_pkgs(`*`<step_upsample>`*`)`](https://themis.tidymodels.org/dev/reference/required_pkgs.step.md)
  : S3 methods for tracking which additional packages are needed for
  steps.
