% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_createClustAll.R
\name{createClustAll}
\alias{createClustAll}
\alias{createClustAll,data.frame,numericOrNA,ANY,characterOrNA-method}
\title{Creates ClustAllObject and perform imputations to deal with missing
values}
\usage{
createClustAll(data=data,
                      nImputation=NULL,
                      dataImputed=NULL,
                      colValidation=NULL)
}
\arguments{
\item{data}{Data Frame of the using data. It may contain missing (NA) values.}

\item{nImputation}{Numeric value with the number of imputations to be
computed in case the data contains NAs.}

\item{dataImputed}{mids object created with mice package. The introduced data
for the imputation and the data using must be the same.}

\item{colValidation}{Character value with the original labelling of the input
data.}
}
\value{
An object of class \code{\link{ClustAllObject-class}}
}
\description{
This pipeline creates the ClustAllObject and computes the imputations if the
dataset contains missing values. The next step would be
\code{\link{runClustAll}}
}
\examples{
# Scenario 1: data does not contain missing values
data("BreastCancerWisconsin", package = "ClustAll")
wdbc <- wdbc[,-c(1,2)]
obj_noNA <- createClustAll(data = wdbc)

# Scenario 2: data contains NAs and imputed data is provided automatically
data("BreastCancerWisconsinMISSING", package = "ClustAll") # load example data
obj_NA <- createClustAll(wdbcNA, nImputation = 5)

# Scenario 3: data contains NAs and imputed data is provided manually
data("BreastCancerWisconsinMISSING", package = "ClustAll") # load the example data
ini <- mice::mice(wdbcNA, maxit = 0, print = FALSE)
pred <- ini$pred # predictor matrix
pred["radius1", c("perimeter1", "area1", "smoothness1")] <- 0 # example of how to remove predictors
imp <- mice::mice(wdbcNA, m=5, pred=pred, maxit=5, seed=1234, print=FALSE)
obj_imp <- createClustAll(data=wdbcNA, dataImputed = imp)

}
\seealso{
\code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
}
