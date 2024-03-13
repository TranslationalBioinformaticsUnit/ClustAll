# createClustAll_internal functions definition ---------------------------------
# These functions are not meant to be invoked directly by the user.
# See the createClusteAll function instead.


# This functions checks if the data frame introduced contains character vectors.
# In that case, it transforms them into numerical values assuming they are categorical values.
checkDataIntroduced <- function(data) {
    if (any(sapply(data, is.character))) {
        message("The dataset contains character values.")
        message("They are converted to categorical (more than one class) or to binary (one class).")
        data_new <- data
        must_convert_variables <- which(sapply(data, is.character))

        if (length(must_convert_variables) == 1) {
            if (isBinary(data[,must_convert_variables])) {
                variables_binary <- must_convert_variables
                variables_categorical <- NULL
            } else {
                variables_binary <- NULL
                variables_categorical <- must_convert_variables
            }
        } else {
            variables_binary <- base::names(which(sapply(data[,must_convert_variables],
                                                    isBinary)))
            variables_categorical <- base::names(which(!sapply(data[,must_convert_variables],
                                                                isBinary)))
            }

        if (length(variables_binary) == 1) {
            data_new[, variables_binary] <- as.numeric(as.factor(data_new[,
                                                            variables_binary]))
        } else if (length(variables_binary) > 1) {
            data_new[, variables_binary] <- sapply(data_new[, variables_binary],
                                        function(x) as.numeric(as.factor(x)))
        }

        if (length(variables_categorical > 1)) {
        message("Categorical variables detected! Applying One Hot enconding...")
            for (variable_name in variables_categorical) {
                onehot_variables <- base::names(table(data[, variable_name]))
                for (new_onehot_variable in onehot_variables) {
                    data_new[, paste0(variable_name, "_", new_onehot_variable)] <- rep(0, nrow(data_new))
            data_new[(which(data_new[, variable_name] == new_onehot_variable)),
                        paste0(variable_name, "_", new_onehot_variable)] <- 1
                data_new[(which(is.na(data_new[, variable_name]))),
                        paste0(variable_name, "_", new_onehot_variable)] <- NA
                }
                data_new <- subset(data_new,
                        select = -which(colnames(data_new) == variable_name))
            }
        }

    } else {
        data_new <- data
    }

    message("Before continuing, check that the transformation has been processed correctly.\n")
    return(data_new)
}


# Check if data is binary
isBinary <- function(vector) {
    if (length(table(vector)) == 2) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


# This functions checks if the vector introduced contains character vectors.
# In that case, it transforms them into numerical values assuming they are
# categorical values.
checkVectorIntroduced <- function(data) {
    if (any(sapply(data, is.character))) {
        message("The dataset contains character values.")
        message("They are converted to categorical (more than one class) or to binary (one class).")
        message("Before continuing, check that the transformation has been processed correctly.\n")

        data_new <- data
        data_new <- as.numeric(as.factor(data_new))

    } else {
        data_new <- data
    }

    return(data_new)
}


# This function checks the number of imputations is valid
validnImputation <- function(nImputation, imputedNull) {
    if (!is.numeric(nImputation) & !is.null(nImputation)) {
        if (imputedNull == FALSE) {
            message("Argument nImputation is not valid.")
            message("Please introduce a positive number of imputations.")
            stop()
        }
    } else if (is.null(nImputation) & imputedNull == TRUE) {
      nImputation <- 0
    } else if (nImputation < 0) {
        message("Argument nImputation is not valid.")
        message("Please introduce a positive number of imputations.")
        stop()
    } else if (nImputation %% 1 != 0) { # check it is not float value
        message("Decimal number included, the decimal value will be rounded.")
        nImputation <- round(nImputation, digits = 0)
    }

    return(nImputation)
}


# This function checks if the original dataset contains missing values and
# the arguments introduced to createClustAll
validData <- function(data, nImputation, dataImputed) {
    if (anyNA(data) == TRUE & nImputation == 0 & is.null(dataImputed)) {
        message("The dataset contains NA values.")
        message("Specify the number of imputations (nImputation) to be computed.")
        stop()

    } else if (anyNA(data) == FALSE & nImputation > 0 & is.null(dataImputed)) {
        message("The dataset does NOT contain NA values.")
        message("The imputation process will not be applied.")
        return(TRUE)

    } else if (anyNA(data) == FALSE  & !is.null(dataImputed)) {
        message("The dataset does NOT contain NA values.")
        message("The imputed data introduced will not be used.")
        return(TRUE)

    } else {
        return(FALSE)
    }
}


# This function checks that the dataImputed manually is mids class.
# It also checks that ClustALL input data and the data used for imputation
# is the same
validDataImputed <- function(data, dataImputed, dataOriginal) {
    if (is(dataImputed, "mids")) {
        if (identical(data, dataImputed$data) | identical(dataOriginal, dataImputed$data)){
            return(TRUE)

        } else {
          message("The input data and the data used for imputation are different.")
          message("Please, make sure you are using the same.")
          stop()
        }

    } else {
        message("You must introduce mice::mice function output.")
        message("For more information visit mice package.")
        stop()
    }
}


checkColumn <- function(data, colValidation) {
    if (!is(colValidation, "character")) {
        message("Please make sure to introduce the name of the original labelling from the input data if present.")
        stop()
    }
    if (!colValidation %in% colnames(data)) {
        message("The introduced columnname is not present in the dataset.")
        message("Please, make sure to introduce it correctly.")
        stop()
    }
}
# END OF ClustAll_createClustAll_internal.R
