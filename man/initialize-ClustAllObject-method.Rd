% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_ClustAllObject_Class.R
\name{initialize,ClustAllObject-method}
\alias{initialize,ClustAllObject-method}
\title{initializeClustAllObject}
\usage{
\S4method{initialize}{ClustAllObject}(
  .Object,
  data,
  dataOriginal,
  dataImputed,
  dataValidation,
  nImputation,
  processed,
  summary_clusters,
  JACCARD_DISTANCE_F
)
}
\arguments{
\item{.Object}{initializing object}

\item{data}{Data Frame of the data used. Maybe modified from the input
data.}

\item{dataOriginal}{Data Frame of the original data introduced.}

\item{dataImputed}{Mids object derived from the
mice package that stores the imputed data, in case
imputation was applied. Otherwise NULL.}

\item{dataValidation}{labelling numericOrNA. Original data labelling.}

\item{nImputation}{Number of multiple imputations to be applied.}

\item{processed}{Logical if the ClustAll pipeline has been executed previously}

\item{summary_clusters}{listOrNULL. List with the resulting stratifications
for each combination of clustering methods (distance + clustering algorithm)
and depth, in case ClustAll pipeline has been executed previously.
Otherwise NULL.}

\item{JACCARD_DISTANCE_F}{matrixOrNULL. Matrix containing the Jaccard
distances derived from the robust populations stratifications if ClustAll
pipeline has been executed previously. Otherwise NULL.}
}
\value{
An object of class \code{\link{ClustAllObject-class}}
}
\description{
constuctor for \code{\link{ClustAllObject-class}}
}
