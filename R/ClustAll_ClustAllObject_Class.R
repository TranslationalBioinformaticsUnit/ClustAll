# setClassUnion includes the new classes defined -------------------------------

#' Class union of list, null or missing
#' @title Class Union listOrNULL
#' @aliases listOrNULL-class
#' @name listOrNULL
#' @description Contains either list, NULL or missing object
#' @return listOrNULL class object
#' @exportClass listOrNULL
#' @export
setClassUnion("listOrNULL", c("list", "NULL", "missing"))


#' Class union of numeric, null or missing
#' @title Class Union numericOrNA
#' @aliases numericOrNA-class
#' @name numericOrNA
#' @description Contains either numeric, NULL or missing object
#' @return numericOrNA class object
#' @exportClass numericOrNA
#' @export
setClassUnion("numericOrNA", c("numeric", "missing", "NULL"))


#' @title characterOrNA
#' Class union of character, null or missing
#' @aliases characterOrNA-class
#' @name characterOrNA
#' @description Contains either character, NULL or missing object
#' @return characterOrNA class object
#' @exportClass characterOrNA
#' @export
setClassUnion("characterOrNA", c("character", "missing", "NULL"))


#' Class union of logical, null or missing
#' @title logicalOrNA
#' @aliases logicalOrNA-class
#' @name logicalOrNA
#' @description Contains either logical, NULL or missing object
#' @return logicalOrNA class object
#' @exportClass logicalOrNA
#' @export
setClassUnion("logicalOrNA", c("logical", "missing", "NULL"))


#' Class union of matrix, null or missing
#' @title matrixOrNULL
#' @aliases matrixOrNULL-class
#' @name matrixOrNULL
#' @description Contains either matrix or NULL object
#' @return matrixOrNULL class object
#' @exportClass matrixOrNULL
#' @export
setClassUnion("matrixOrNULL", c("matrix", "NULL"))


#' Class union of numericor character
#' @title numericOrCharacter
#' @aliases numericOrCharacter-class
#' @name numericOrCharacter
#' @description Contains either numeric or character object
#' @return numericOrCharacter class object
#' @exportClass numericOrCharacter
#' @export
numericOrCharacter <- setClassUnion("numericOrCharacter",
                                    c("numeric", "character"))


# ClustAllObject Class ---------------------------------------------------------
#' @title ClustAllObject
#' @aliases ClustAllObject-class
#' @description Stores the original data used, the imputed datasets and the
#' results of the ClustAll pipeline.
#' @slot data Data Frame of the data used. Maybe modified from the input
#' data.
#' @slot dataOriginal Data Frame of the original data introduced.
#' @slot dataImputed  Mids object derived from the
#' mice package that stores the imputed data, in case
#' imputation was applied. Otherwise NULL.
#' @slot dataValidation labelling numericOrNA. Original data labelling.
#' @slot nImputation Number of multiple imputations to be applied.
#' @slot processed Logical if the ClustAll pipeline has been executed previously
#' @slot summary_clusters listOrNULL. List with the resulting stratifications
#' for each combination of clustering methods (distance + clustering algorithm)
#' and depth, in case ClustAll pipeline has been executed previously.
#' Otherwise NULL.
#' @slot JACCARD_DISTANCE_F matrixOrNULL. Matrix containing the Jaccard
#' distances derived from the robust populations stratifications if ClustAll
#' pipeline has been executed previously. Otherwise NULL.
#' @return ClustAllObject class object
#' @exportClass ClustAllObject
#' @export
setClass(
  Class="ClustAllObject",
  slots=list(
    data="data.frame",
    dataOriginal="data.frame",
    dataImputed="ANY",
    dataValidation="numericOrNA",
    nImputation ="numeric",
    processed="logical",
    summary_clusters="listOrNULL",
    JACCARD_DISTANCE_F="matrixOrNULL"
  )
)


# ClustAllObject constructor
#' constuctor for \code{\link{ClustAllObject-class}}
#' @title initializeClustAllObject
#' @param .Object initializing object
#' @param data Data Frame of the data used. Maybe modified from the input
#' data.
#' @param dataOriginal Data Frame of the original data introduced.
#' @param dataImputed  Mids object derived from the
#' mice package that stores the imputed data, in case
#' imputation was applied. Otherwise NULL.
#' @param dataValidation labelling numericOrNA. Original data labelling.
#' @param nImputation Number of multiple imputations to be applied.
#' @param processed Logical if the ClustAll pipeline has been executed previously
#' @param summary_clusters listOrNULL. List with the resulting stratifications
#' for each combination of clustering methods (distance + clustering algorithm)
#' and depth, in case ClustAll pipeline has been executed previously.
#' Otherwise NULL.
#' @param JACCARD_DISTANCE_F matrixOrNULL. Matrix containing the Jaccard
#' distances derived from the robust populations stratifications if ClustAll
#' pipeline has been executed previously. Otherwise NULL.
#' @return An object of class \code{\link{ClustAllObject-class}}
setMethod(
  f="initialize",
  signature="ClustAllObject",
  function(.Object,
           data,
           dataOriginal,
           dataImputed,
           dataValidation,
           nImputation,
           processed,
           summary_clusters,
           JACCARD_DISTANCE_F) {
    .Object@data <- data
    .Object@dataOriginal <- data
    .Object@dataImputed <- dataImputed
    .Object@nImputation <- nImputation
    .Object@dataValidation <- dataValidation
    .Object@processed <- FALSE
    .Object@summary_clusters <- NULL
    .Object@JACCARD_DISTANCE_F <- NULL
    validObject(.Object)

    return(.Object)
  }
)



# ClustAllObject MEMBER ACCESS METHODS -----------------------------------------

#' Show method for a \code{\link{ClustAllObject-class}} object
#' @title show method for ClustAllObject
#' @param object \code{\link{ClustAllObject-class}} object
#' @return summarize information about the object
setMethod("show", "ClustAllObject", function(object) {

    cat(is(object)[[1]], "\n",
        "Data: Number of variables: ",ncol(object@data),
        ". Number of patients: ", nrow(object@data), "\n",
        "Imputation: ", ifelse(is.null(object@dataImputed), "NO.", "YES."),
        "\nNumber of imputations: ", object@nImputation, "\n",
        "Processed: ", object@processed, "\n",
        "Number of stratifications: ", ifelse(object@processed,
                                             nrow(object@JACCARD_DISTANCE_F),
                                             "NULL"), "\n", sep = ""
    )
  }
)

#' @title Retrieve the initial data from ClustAllObject
#' @aliases showData,ClustAllObject-method
#' @description
#' Generic function to retrieve the initial data used for
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return The Data Frame with the initial data
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' showData(obj_noNA)
#' @export
setGeneric(
  name="showData",
  def=function(Object){standardGeneric("showData")}
)

setMethod(
  f="showData",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@data}
)

#' @title Retrieve the initial dataOriginal from ClustAllObject
#' @aliases dataOriginal,ClustAllObject-method
#' @description
#' Generic function to retrieve the initial data used for
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage dataOriginal(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return The Data Frame with the initial data
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' dataOriginal(obj_noNA)
#' @export
setGeneric(
  name="dataOriginal",
  def=function(Object){standardGeneric("dataOriginal")}
)

setMethod(
  f="dataOriginal",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@dataOriginal}
)

#' @title Retrieve the imputed data from ClustAllObject
#' @aliases dataImputed,ClustAllObject-method
#' @description
#' Generic function to retrieve the imputed data obtained in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage dataImputed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Mids class object with the imputed data or NULL if imputation was
#' not required
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_NA <- createClustAll(data = wdbcNA, colValidation = "Diagnosis",
#'                          dataImputed = wdbcMIDS)
#' dataImputed(obj_NA)
#' @export
setGeneric(
  name="dataImputed",
  def=function(Object){standardGeneric("dataImputed")}
)

setMethod(
  f="dataImputed",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@dataImputed}
)


#' @title Retrieve the number of imputations applied at the imputation step from
#' ClustAllObject
#' @aliases nImputation,ClustAllObject-method
#' @description
#' Generic function to retrieve the number of imputations in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage nImputation(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Numeric vector that contains the number of imputations. 0 in the
#' case of no imputations were required
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_NA <- createClustAll(data = wdbcNA, colValidation = "Diagnosis",
#'                          dataImputed = wdbcMIDS)
#' nImputation(obj_NA)
#' @export
setGeneric(
  name="nImputation",
  def=function(Object){standardGeneric("nImputation")}
)

setMethod(
  f="nImputation",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@nImputation}
)


#' @title Retrieve the resulting stratifications for each combination of
#' clusterings method (distance + clustering algorithm) and depth from
#' ClustAllObject
#' @aliases summary_clusters,ClustAllObject-method
#' @description
#' Generic function to retrieve the resulting stratifications for each
#' combination of clusterings method (distance + clustering algorithm) and
#' depth of \code{\link{runClustAll}} from a \code{\link{ClustAllObject-class}}
#' object
#' @usage summary_clusters(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return List with the resulting stratifications for each combination of
#' clusterings method (distance + clustering algorithm) and depth methods or
#' NULL if runClustAll method has not been executed yet.
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = FALSE)
#' summary_clusters(obj_noNA1)
#' @export
setGeneric(
  name="summary_clusters",
  def=function(Object){standardGeneric("summary_clusters")}
)

setMethod(
  f="summary_clusters",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@summary_clusters}
)


#' @title Retrieve the matrix with the Jaccard distances derived from the robust
#' populations stratifications in ClustAllObject
#' @aliases JACCARD_DISTANCE_F,ClustAllObject-method
#' @description
#' Generic function to retrieve the matrix with the Jaccard distances derived
#' from the robust populations stratifications in\code{\link{runClustAll}} from
#' a \code{\link{ClustAllObject-class}} object
#' @usage JACCARD_DISTANCE_F(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Matrix containing the Jaccard distances derived from the robust
#' populations stratifications or NULL if runClustAll method has not been
#' executed yet
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = FALSE)
#' JACCARD_DISTANCE_F(obj_noNA1)
#' @export
setGeneric(
  name="JACCARD_DISTANCE_F",
  def=function(Object){standardGeneric("JACCARD_DISTANCE_F")}
)

setMethod(
  f="JACCARD_DISTANCE_F",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@JACCARD_DISTANCE_F}
)


#' @title Retrieve logical if runClustAll has been executed considering
#' ClustAllObject as input
#' @aliases processed,ClustAllObject-method
#' @description
#' Generic function to retrieve the logical if \code{\link{runClustAll}} have
#' been runned from a \code{\link{ClustAllObject-class}} object
#' @usage processed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return TRUE if runClustAll has been already executed. Otherwise FALSE
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' processed(obj_noNA)
#' @export
setGeneric(
  name="processed",
  def=function(Object){standardGeneric("processed")}
)

setMethod(
  f="processed",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@processed)
  }
)


#' @title Retrieve the original data labelling from ClustAllObject
#' @aliases dataValidation,ClustAllObject-method
#' @description
#' Generic function to retrieve numeric vector if it has been added with the
#' true labels from a \code{\link{ClustAllObject-class}} object
#' @usage dataValidation(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return numeric vector if true labels have been added. Otherwise NULL
#' @seealso \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_noNA <- createClustAll(data = wdbc, colValidation="Diagnosis")
#' dataValidation(obj_noNA)
#' @export
setGeneric(
  name="dataValidation",
  def=function(Object){standardGeneric("dataValidation")}
)

setMethod(
  f="dataValidation",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@dataValidation}
)

#' addValidationData
#' @title Add the validation data into the ClustAllObject
#' @name addValidationData
#' @docType methods
#' @rdname addValidationData
#' @aliases addValidationData,ClustAllObject,numericOrCharacter-method
#' @description
#' Generic function to add validation data to the
#' \code{\link{ClustAllObject-class}} object
#' @usage addValidationData(Object, dataValidation)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param dataValidation numericOrCharacter
#' @return \code{\link{ClustAllObject-class}} object
#' @seealso \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1, 2)] # delete patients IDs & label
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA <- addValidationData(Object = obj_noNA,
#'                               dataValidation = label)
#' @exportMethod addValidationData
#' @export
setGeneric(
  name="addValidationData",
  def=function(Object, dataValidation){standardGeneric("addValidationData")}
)

setMethod(
  f="addValidationData",
  signature=signature(
    Object="ClustAllObject",
    dataValidation="numericOrCharacter"),
  definition=function(Object, dataValidation) {

    if (!is.null(Object@dataValidation)) {
      message("The object already has a labelling.")
      message("Rewriting the labelling data...")
    }

    dataValidation <- checkVectorIntroduced(dataValidation)

    if (length(dataValidation) != nrow(Object@data)) {
      message("The introduced data  and the original data labelling have different lenghts.")
      message("Make sure the introduced data is correct.")
      stop()
    }

    Object@dataValidation <- dataValidation
    return(Object)
  }
)


# Cocumenting DataSets ---------------------------------------------------------
#' wdbc: Diagnostic Wisconsin Breast Cancer Database.
#'
#' A dataset containing Features are computed from a digitized image of a fine
#' needle aspirate (FNA) of a breast mass.
#' They describe characteristics of the cell nuclei present in the image.
#'
#' The dataset comprises two types of features —categorical and numerical—
#' derived from a digitized image of a fine needle aspirate (FNA) of a breast
#' mass from 659 patients. Each patient is characterized by 31 features (10x3)
#' and belongs to one of two target classes: ‘malignant’ or ‘benign’.
#' @source <https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic>
#'
#' \itemize{
#'   \item Diagnosis Label says tumor is malingnant or benignant
#'   \item radius. Mean of distances from the center to points on the perimeter
#'   \item perimeter
#'   \item area
#'   \item smoothness. Local variation in radius lengths
#'   \item compactness. (Perimeter^2 / Area) - 1.0
#'   \item concavity. Severity of concave portions of the contour
#'   \item concave points. Number of concave portions of the contour
#'   \item symmetry.
#'   \item fractal dimension. “Coastline approximation” - 1.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name wdbc
#' @usage data("BreastCancerWisconsin", package = "ClustAll")
#' @format A data frame with 660 rows and 31 variables
#' @return wdbc dataset
NULL

#' wdbcNA: Diagnostic Wisconsin Breast Cancer Database with missing values
#'
#' We introduced random missing values to the wdbc dataset. \code{\link{wdbc}}
#'
#' @docType data
#' @keywords datasets
#' @name wdbcNA
#' @usage data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' @format A data frame with 660 rows and 31 variables
#' @return wdbcNA dataset
NULL

#' wdbcMIDS: Diagnostic Wisconsin Breast Cancer Database with imputed values
#'
#' We introduced imputed random values to the wdbcNA dataset.
#' Using Mice. It is a mids object. \code{\link{wdbc}}
#'
#' @docType data
#' @keywords datasets
#' @name wdbcMIDS
#' @usage data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' @format A data frame with 660 rows and 31 variables
#' @return wdbcMIDS dataset
NULL

#' obj_noNA1: Processed wdbc dataset for testing purposed
#'
#' Processed wdbc as appear in vignette
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNA1
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

#' obj_noNA1simplify: Processed wdbc dataset for testing purposed
#'
#' Processed wdbc as appear in vignette, with simplify TRUE parameter
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNA1simplify
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

#' obj_noNAno1Validation: Processed wdbc dataset for testing purposed
#'
#' Processed wdbc as appear in vignette, with no validation data
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNAno1Validation
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

# END OF ClustAll_ClustAllObject_Class.R
