#' @details
#' ADASYN is an extension of SMOTE that adaptively generates synthetic minority
#' class examples based on the local distribution of each minority instance.
#' Instead of generating the same number of synthetic examples for every
#' minority instance, ADASYN generates more synthetic examples for instances
#' that are harder to learn, specifically those surrounded by more majority
#' class neighbors. This focuses synthetic data generation on the difficult
#' boundary regions of the minority class, resulting in a more informative
#' augmentation than standard SMOTE.
