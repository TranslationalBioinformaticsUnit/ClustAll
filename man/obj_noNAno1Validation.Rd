% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_ClustAllObject_Class.R
\docType{data}
\name{obj_noNAno1Validation}
\alias{obj_noNAno1Validation}
\title{obj_noNAno1Validation: Processed wdbc dataset for testing purposed}
\format{
A processed ClustAllObject
}
\usage{
data("testData", package = "ClustAll")
}
\value{
ClustAllObject Object
}
\description{
The "obj_noNAno1Validation" dataset is a processed version of the Breast
Cancer Wisconsin (Diagnostic) dataset (\code{\link{wdbc}}), prepared for
testing purposes and used in the ClustAll package vignette. It has been
processed using the ClustAll package methods, but with the validation data
removed.
}
\details{
The "obj_noNAno1Validation" dataset can be used as a reference for users who want
to understand the ClustAll package workflow and replicate the analysis presented
in the vignette, focusing on unsupervised learning aspects.

For more information on the original dataset and the processed version with
validation data, please refer to the documentation of the "wdbc"
(\code{\link{wdbc}}).
}
\keyword{datasets}
