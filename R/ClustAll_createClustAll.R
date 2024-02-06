# createClustAll ---------------------------------------------------------------
#' @title Creates ClustAllObject and perform imputations to deal with missing
#' values
#' @aliases createClustAll,data.frame,numericOrNA,ANY,characterOrNA-method
#' @description
#' This pipeline creates the ClustAllObject and computes the imputations if the
#' dataset contains missing values. The next step would be
#' \code{\link{runClustAll}}
#' @usage createClustAll(data=data,
#'                       nImputation=NULL,
#'                       dataImputed=NULL,
#'                       colValidation=NULL)
#' @param data Data Frame of the using data. It may contain missing (NA) values.
#' @param nImputation Numeric value with the number of imputations to be
#' computed in case the data contains NAs.
#' @param dataImputed mids object created with mice package. The introduced data
#' for the imputation and the data using must be the same.
#' @param colValidation Character value with the original labelling of the input
#' data.
#' @return An object of class \code{\link{ClustAllObject-class}}
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' # Scenario 1: data does not contain missing values
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#'
#' # Scenario 2: data contains NAs and imputed data is provided automatically
#' data("BreastCancerWisconsinMISSING", package = "ClustAll") # load example data
#' obj_NA <- createClustAll(wdbcNA, nImputation = 5)
#'
#' # Scenario 3: data contains NAs and imputed data is provided manually
#' data("BreastCancerWisconsinMISSING", package = "ClustAll") # load the example data
#' ini <- mice::mice(wdbcNA, maxit = 0, print = FALSE)
#' pred <- ini$pred # predictor matrix
#' pred["radius1", c("perimeter1", "area1", "smoothness1")] <- 0 # example of how to remove predictors
#' imp <- mice::mice(wdbcNA, m=5, pred=pred, maxit=5, seed=1234, print=FALSE)
#' obj_imp <- createClustAll(data=wdbcNA, dataImputed = imp)
#'
#' @export
setGeneric(
  name="createClustAll",
  def=function(data=data, nImputation=NULL, dataImputed=NULL, colValidation=NULL)
  {standardGeneric("createClustAll")}
)

setMethod(
  f="createClustAll",
  signature=signature(
    data="data.frame",
    nImputation="numericOrNA",
    dataImputed="ANY",
    colValidation="characterOrNA"),
  definition=function(data=data, nImputation=NULL,
                      dataImputed=NULL, colValidation=NULL) {
    dataOriginal <- data
    if (is.null(dataImputed)) {imputedNull <- TRUE} else {imputedNull <- FALSE}
    if (is.null(nImputation)) {nImputation <- 0}
    data <- checkDataIntroduced(data)
    nImputation <- validnImputation(nImputation, imputedNull)

    if (!is.null(colValidation)) {
      checkColumn(data, colValidation)
      dataValidation <- data[, colValidation]
      data <- subset(data, select = -which(colnames(data)==colValidation))
    } else {
      dataValidation <- NULL
    }

    if (validData(data, nImputation, dataImputed)) {nImputation <- 0}
    if (nImputation == 0 & is.null(dataImputed)) {
      dataImputed <- NULL
    } else if (is.null(dataImputed)) {
      message("Running default multiple imputation method.")
      message("For more information check mice package.")
      dataImputed <- mice(data, m=nImputation, maxit=5, seed=1234)
    } else if (!is.null(dataImputed)) {
      # check the imputed data
      if (validDataImputed(data=data, dataImputed=dataImputed,
                           dataOriginal=dataOriginal)) {
        nImputation <- dataImputed$m
      }
    }
    clustAllObj <- new("ClustAllObject",data=data,dataOriginal=dataOriginal,
                       dataImputed=dataImputed, nImputation=nImputation,
                       dataValidation=dataValidation)
    message("\nClustALL object created successfully. You can use runClustAll.")

    return(clustAllObj)
  }
)
# END OF ClustAll_createClustAll.R
