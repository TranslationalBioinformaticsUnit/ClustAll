data("testData", package = "ClustAll")

# Test obtain expected result for a certain similarity
test_defClusters <- function() {
  expected_result <- c("cuts_a_28", "cuts_c_9",  "cuts_c_4",  "cuts_b_13")
  result <- resStratification(Object=obj_noNA1, stratification_similarity=0.9,
                              all = F)

  checkEquals(names(result), expected_result)
}

# Test for validation data does not exist
test_validationData <- function() {
  validateStratification(obj_noNA1, stratificationName = "cuts_b_13")
  checkException(validateStratification(obj_noNA1, stratificationName = "NOTexist"))
  checkException(plotSANKEY(obj_noNA1, clusters = c("cuts_b_13","NOTexist")))
  checkException(cluster2data(obj_noNA1, stratificationName = "NOTexist"))

  checkException(plotSANKEY(obj_noNAno1Validation, clusters = "cuts_b_13", validationData = TRUE)) # exception as validation data is not included
  checkException(validateStratification(obj_noNAno1Validation, stratificationName = "cuts_b_13")) # exception as validation data is not included
}


# Internal functions -----------------------------------------------------------
# ## This functions are for testing internal functions from runClustAll.
# ##As these function are not exported cannot be tested.
# ## This tests are for internal development.
#
# # source("R/ClustAll_ClustAllObject_Methods_internal.R")
#
# # Test for obtainDefCluster
# test_obtainDefCluster <- function() {
#   mat_using <- matrix(c(1, 0.75, 0.4, 0.11, 0.75, 1, 0.12 , 0.12, 0.4, 0.12, 1, 0.13, 0.11, 0.12, 0.13, 1), nrow = 4)
#   colnames(mat_using) <- c("Cluster_1", "Cluster_2", "Cluster_3", "Cluster_4")
#   rownames(mat_using) <- c("Cluster_1", "Cluster_2", "Cluster_3", "Cluster_4")
#
#   cluster_similarity <- 0.7
#   expected_result <- list(c("Cluster_1", "Cluster_2"))
#   result <- obtainDefCluster(mat_using, cluster_similarity)
#   checkEquals(result, expected_result)
# }
#
# # Test for chooseClusters, that obtains the correctly the clusters taking into account the population
# test_chooseClusters <- function() {
#   definitive_clusters <- list(c("Cluster_1", "Cluster_2"))
#   summary_clusters <- list("Cluster_1" = c(1, 1, 1, 2), "Cluster_2" = c(1, 2, 2, 1))
#   population1 <- 0.15
#   population2 <- 0.35
#   expected_result <-
#   resultAll <- chooseClusters(definitive_clusters, summary_clusters, population1, all=TRUE)
#   resultNoAll <- chooseClusters(definitive_clusters, summary_clusters, population1, all=FALSE)
#   resulPopulation <- chooseClusters(definitive_clusters, summary_clusters, population2, all=TRUE)
#   checkEquals(resultAll, list(c("Cluster_1", "Cluster_2"))) # Expected to obtain all the result
#   checkEquals(resultNoAll, list("Cluster_1")) # Expected to obtain only the representative
#   checkEquals(resulPopulation, list("Cluster_2")) # Population is 35% expected only obtain Cluster_2
# }
#
# # Test checkCluster, checks that the cluster exist
# test_checkCluster <- function() {
#   lCluster <- c("Cluster_1", "Cluster_2")
#   names(lCluster) <- lCluster
#   checkCluster("Cluster_1", lCluster)
#   checkException(checkCluster("Cluster_3", lCluster))
# }
#
# # Test for obtain_metadata, extracts the metadata correctly
# test_obtain_metadata <- function() {
#   set.seed(123)
#   data <- matrix(runif(16), nrow = 4, ncol = 4)
#   colnames(data) <- c("cuts_a_1", "cuts_b_2", "cuts_c_3", "cuts_d_4")
#   rownames(data) <- c("cuts_a_1", "cuts_b_2", "cuts_c_3", "cuts_d_4")
#
#   expected_result <- data.frame("Distance" = c("Correlation", "Correlation", "Gower", "Gower"),
#                                 "Clustering" = c("Hierarchical", "k-means", "Hierarchical", "k-medoids"),
#                                 "Depth" = c(1, 2, 3, 4))
#   result <- obtain_metadata(data)
#   checkEquals(result, expected_result)
# }
