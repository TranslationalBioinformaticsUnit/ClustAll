###########################################
# All possibilities to create the object
###########################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll")
data("BreastCancerWisconsinMISSING", package = "ClustAll")

test_creatingClustAll1 <- function() {
  # Scenario 1: 1.No NA, 2.No imputation, 3.No imputation implemented
  createClustAll(data = wdbc, colValidation = "Diagnosis",
                 nImputation = NULL, dataImputed = NULL)
}

test_creatingClustAll2 <- function() {
  # Scenario 2: 1.Yes NA, 2.Yes imputation, 3.Imputations automatically (default)
  createClustAll(wdbcNA, nImputation = 2,  colValidation = "Diagnosis")
  createClustAll(wdbcNA, dataImputed = wdbcMIDS,
                 colValidation = "Diagnosis")
}

test_creatingClustAll3 <- function() {
  # Scenario 3: 1.Yes NA, 2.Yes imputation, 3.Imputations manually
  createClustAll(wdbcNA, dataImputed = wdbcMIDS,
                 colValidation = "Diagnosis")
}
