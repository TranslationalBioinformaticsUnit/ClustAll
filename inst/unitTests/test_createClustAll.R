require(mice)

# Unit tests for the introduced data is correct, and transformed correctly
test_introducedData <- function() {
  data_binary <- data.frame(a = c(1, 2, 3, 4), b = c("A", "B", "A", "B")) # b is binary
  data_categorical <- data.frame(a = c(1, 2, 3, 4), b = c("A", "B", "A", "B"), # b is binary and c has 3 categories
                                 c = c("A", "B", "A", "C"))
  data_categorical2 <- data.frame(a = c(1, 2, 3, 4), b = c("A", "B", "A", "B"), # b is binary and c has 4 categories
                                  c = c("A", "B", "C", "D"))

  obj_binary <- createClustAll(data=data_binary)
  obj_categorical <- createClustAll(data=data_categorical)
  obj_categorical2 <- createClustAll(data=data_categorical2)

  checkEquals(ncol(obj_binary@data), ncol(data_binary)) # Expected same number of columns as the introduced data (b) is binary

  additional_columns <- length(unique(data_categorical$c)) - 1  # In one hot encoding expected one binary column for each categorical value
  checkEquals(ncol(data_categorical) + additional_columns, ncol(obj_categorical@data))

  additional_columns2 <- length(unique(data_categorical2$c)) - 1  # In one hot encoding expected one binary column for each categorical value
  checkEquals(ncol(data_categorical2) + additional_columns2, ncol(obj_categorical2@data))

  checkTrue(!any(sapply(obj_binary@data, is.character)))  # There should be no categorical variables after the transformation
  checkTrue(!any(sapply(obj_categorical@data, is.character)))  # There should be no categorical variables after the transformation
}

# Unit tests for valid number of imputations
test_nImputation <- function() {
  data_NA <- data.frame(a = c(1, 2, 3, 4), b = c("A", "B", "A", "B"),
                        c = c("A", "B", "A", NA))
  data_noNA <- data.frame(a = c(1, 2, 3, 4), b = c("A", "A", "A", "B"),
                          c = c("A", "B", "A", "B"))
  objnoNA <- createClustAll(data=data_noNA, nImputation = 2) # When no NA values no imputations happens
  checkEquals(objnoNA@nImputation, 0) # Even we specified 2 imputations should not have as it not contains NAs

  obj_NAn1 <- createClustAll(data=data_NA, nImputation = 2)
  obj_NAn25 <- createClustAll(data=data_NA, nImputation = 2.5) # round down
  obj_NAn26 <- createClustAll(data=data_NA, nImputation = 2.6) # round up

  checkEquals(obj_NAn1@nImputation, 2)
  checkEquals(obj_NAn25@nImputation, 2)
  checkEquals(obj_NAn26@nImputation, 3)

  checkException(createClustAll(data=data_NA, nImputation = 0)) # 0 imputation are not allowed when there are NAs
  checkException(createClustAll(data=data_NA, nImputation = -1)) # negative values are not allowed
  checkException(createClustAll(data=data_NA, nImputation = "NOTnumber")) # not numeric vector allowed
}

# Unit tests when imputations are done manually
test_ValidImputation <- function() {
  data_NA <- data.frame(a = c(1, 2, 3, 4), b = c("A", "B", "A", "B"),
                        c = c("A", "B", "A", NA))

  dataImputed <- mice::mice(data_NA, m=2, maxit=5, seed=123) # manual imputation
  obj_imputed <- createClustAll(data_NA, dataImputed = dataImputed)  # Expected it works as the original data is the same

  # data_NAchanged <- data_NA <- data.frame(a = c(2, 2, 3, 4), b = c("A", "B", "A", "B"), # we changed the first element of a column 1->2
  #                                         c = c("A", "B", "A", NA))
  # dataImputedchanged <- mice::mice(data_NAchanged, m=2, maxit=5, seed=123) # manual imputation
  # checkException(createClustAll(data=data_NA, dataImputed=dataImputedchanged)) # exception expected as the data frame
  #                                                                              # and the original data of imputations are different
}

test_addValidationData <- function() {
  data_validation <- data.frame(a = c(1, 2, 3, 4), b = c("A", "B", "A", "B"), # b is binary and c has 3 categories
                                c = c("A", "B", "A", "A"), d = c(1, 1, 1, 2)) # d is validation data
  obj_validation <- createClustAll(data=data_validation, colValidation = "d")
  checkEquals(ncol(obj_validation@data), ncol(data_validation)-1) # expected one column less as the validation column has been substracted

  checkException(createClustAll(data=data_validation, colValidation = "NOTexist")) # introduced a not existing validation data

  validation <- c(1, 1, 1, 2)
  incorrectValidation <- c(1, 1, 1, 2, 1) # more elements than patients

  obj_noValidation <- createClustAll(data_validation)
  addValidationData(obj_noValidation, validation) # add validation after creating object
  checkException(addValidationData(obj_noValidation, incorrectValidation))
}

