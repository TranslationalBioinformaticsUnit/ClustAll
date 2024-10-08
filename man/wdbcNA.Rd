% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_ClustAllObject_Class.R
\docType{data}
\name{wdbcNA}
\alias{wdbcNA}
\title{wdbcNA: Diagnostic Wisconsin Breast Cancer Database with missing values}
\format{
A data frame with 660 rows and 31 variables
}
\usage{
data("BreastCancerWisconsinMISSING", package = "ClustAll")
}
\value{
wdbcNA dataset
}
\description{
The "wdbcNA" dataset is a modified version of the Breast Cancer Wisconsin
(Diagnostic) dataset (\code{\link{wdbc}}), incorporating missing values at
random. This dataset is designed to demonstrate and evaluate the handling
of missing data in the context of breast cancer diagnosis.
}
\details{
The dataset retains the same structure as the original "wdbc" dataset, with
569 patients and 32 variables. However, some values in the numerical features
have been randomly replaced with missing values (NA).

For a detailed description of the features and the original dataset, please
refer to the documentation of the "wdbc" dataset (\code{\link{wdbc}}).
}
\keyword{datasets}
