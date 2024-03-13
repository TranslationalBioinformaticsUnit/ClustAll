##########################################################
# Running the pipeline with parallelization
##########################################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data
wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
obj <- createClustAll(data = wdbc, colValidation = NULL,
                      nImputation = NULL, dataImputed = NULL)

# This part of the code is commented out as it exceeds the R CMD check time on
# the macOS 12.7.1 Monterey/x86_64 machine.
# example_pipeline1 <- function() {
#   # 1. No parallelization, 2. No simplified (use all depth)
#   runClustAll(obj, threads = 1, simplify = FALSE)
# }
#
# test_pipeline2 <- function() {
#   # 1. No parallelization, 2. YES simplified (one depth every four)
#   runClustAll(obj, threads = 1, simplify = TRUE)
# }
#
# test_pipeline3 <- function() {
#   # 1. Yes parallelization, 2. No simplified (use all depth)
#   runClustAll(obj, threads = 2, simplify = FALSE)
# }
#
# test_pipeline4 <- function() {
#   # 1. No parallelization, 2. YES simplified (one depth every four)
#   runClustAll(obj, threads = 2, simplify = TRUE)
# }

