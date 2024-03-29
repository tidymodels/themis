% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nearmiss_impl.R
\name{nearmiss}
\alias{nearmiss}
\title{Remove Points Near Other Classes}
\usage{
nearmiss(df, var, k = 5, under_ratio = 1)
}
\arguments{
\item{df}{data.frame or tibble. Must have 1 factor variable and remaining
numeric variables.}

\item{var}{Character, name of variable containing factor variable.}

\item{k}{An integer. Number of nearest neighbor that are used
to generate the new examples of the minority class.}

\item{under_ratio}{A numeric value for the ratio of the
majority-to-minority frequencies. The default value (1) means
that all other levels are sampled down to have the same
frequency as the least occurring level. A value of 2 would mean
that the majority levels will have (at most) (approximately)
twice as many rows than the minority level.}
}
\value{
A data.frame or tibble, depending on type of \code{df}.
}
\description{
Generates synthetic positive instances using nearmiss algorithm.
}
\details{
All columns used in this function must be numeric with no missing data.
}
\examples{
circle_numeric <- circle_example[, c("x", "y", "class")]

res <- nearmiss(circle_numeric, var = "class")

res <- nearmiss(circle_numeric, var = "class", k = 10)

res <- nearmiss(circle_numeric, var = "class", under_ratio = 1.5)
}
\references{
Inderjeet Mani and I Zhang. knn approach to unbalanced data
distributions: a case study involving information extraction. In Proceedings
of workshop on learning from imbalanced datasets, 2003.
}
\seealso{
\code{\link[=step_nearmiss]{step_nearmiss()}} for step function of this method

Other Direct Implementations: 
\code{\link{adasyn}()},
\code{\link{bsmote}()},
\code{\link{smote}()},
\code{\link{smotenc}()},
\code{\link{tomek}()}
}
\concept{Direct Implementations}
