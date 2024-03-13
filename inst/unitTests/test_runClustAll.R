data("testData", package = "ClustAll")

# Test to check obtained sratifications with simplified parameter are the same
test_simplify <- function() {
  checkEquals(obj_noNA1@summary_clusters$cuts_a_9,
              obj_noNA1simplify@summary_clusters$cuts_a_9)
  checkEquals(obj_noNA1@summary_clusters$cuts_b_13,
              obj_noNA1simplify@summary_clusters$cuts_b_13)
}

# Internal functions -----------------------------------------------------------
# ## This functions are for testing internal functions from runClustAll.
# ##As these function are not exported cannot be tested.
# ## This tests are for internal development.
# require(bigstatsr)
# require(FactoMineR)
# require(stats)
# require(fpc)
#
# source("R/ClustAll_runClustAll_internal.R")
#
# # Test for createEmptyArrays, create the empty arrays correctly
# test_createEmptyArrays <- function() {
#   data <- matrix(1:9, nrow = 3)
#   nvariables <- 2
#   empty_arrays <- createEmptyArrays(2, data)
#   checkEquals(length(empty_arrays), nvariables) # check there are as many arrays as variables
#   for (i in seq_along(empty_arrays)) {
#     checkEquals(dim(empty_arrays[[i]]), c(nrow(data), nrow(data))) # check dimensions of the arrays/matrix
#     checkTrue(all(empty_arrays[[i]][] == 0))  # check every value is 0
#   }
# }
#
# # Test for obtainDataPCA, generate the PCA correctly
# test_obtainDataPCA <- function() {
#   set.seed(123)
#   data <- matrix(rnorm(50), ncol = 5)
#   colnames(data) <- c("A", "B", "C", "D", "E")
#   rownames(data) <- c(1:10)
#
#   cor_est_var <- cor(data, method="spearman")
#   variables_clust <- hclust(as.dist(cor_est_var))
#   possible_heights <- variables_clust$height
#   heights_cut <- 2
#
#   data_PCA <- obtainDataPCA(data, variables_clust, possible_heights, heights_cut)
#   checkTrue(is(data_PCA, "matrix")) # check it's a matrix
#   checkTrue(ncol(data_PCA) < ncol(data)) # expected less columns as original data as it is a PCA
# }
#
# # Test for obtainImputationData, fill the NA values correctly
# test_obtainImputationData <- function() {
#   set.seed(123)
#   data <- data.frame(matrix(sample(c(1:5, NA), 100, replace = TRUE), ncol = 10))
#   dataImputed <- mice::mice(data, m=2, maxit=5, seed=123)
#   impgo <- 2
#   impData <- obtainImputationData(dataImputed, impgo)
#   checkTrue(is(impData, "data.frame")) # expect obtained a data frame
#   checkEquals(sum(is.na(impData)), 0) # check there are no NAs
#   checkTrue(!anyNA(impData)) # check again there is no NAs
# }
#
# # Test for cstats.table_hclust
# test_cstats.table_hclust <- function() {
#   set.seed(123)
#   dist <- dist(matrix(rnorm(100), ncol = 10))
#   tree <- hclust(dist)
#   k <- 3
#   result <- cstats.table_hclust(dist, tree, k)
#   checkTrue(is(result, "numeric")) # check obtained a numeric vector
#   checkEquals(length(result), 1) # check there is only one result
# }
#
# # Test for cstats.table_PAM, the statistic calculation is correct
# test_cstats.table_PAM <- function() {
#   set.seed(123)
#   dist <- dist(matrix(rnorm(100), ncol = 10))
#   k <- 3
#   result <- cstats.table_PAM(dist, k)
#   checkTrue(is(result, "numeric")) # check obtained a numeric vector
#   checkEquals(length(result), 1) # check there is only one result
# }
#
# # Test for obtainSummaryCluster, summarize correctly
# test_obtainSummaryCluster <- function() {
#   summary_clusters_a <- matrix(9:17, nrow = 3)
#   summary_clusters_b <- matrix(rep(1, 9), nrow = 3)
#   summary_clusters_c <- matrix(1:9, nrow = 3)
#   summary_clusters_d <- matrix(rep(2, 9), nrow = 3)
#   summary_n_clust <- obtainSummaryCluster(summary_clusters_a, summary_clusters_b, summary_clusters_c, summary_clusters_d)
#   checkTrue(is(summary_n_clust, "numeric")) # check all values numeric
#   checkEquals(length(summary_n_clust), 12)  # The length must be the sum of the rows of all matrices
# }
#
# # Test for cluster_similarity_adapt, check that the cluster membership is calculated correctly
# test_cluster_similarity_adapt <- function() {
#   labels1 <- c(1, 1, 2, 2)
#   labels2 <- c(1, 1, 1, 2)
#   labels3 <- c(2, 2, 1, 1)
#   labels4 <- c(1, 2, 1, 2)
#
#   similarity1 <- cluster_similarity_adapt(labels1, labels2, similarity = "jaccard") # expect 0.25
#   checkEquals(similarity1, 0.25)
#   similarity2 <- cluster_similarity_adapt(labels1, labels1, similarity = "jaccard") # expect 1  checkEquals(similarity1, 0.25)
#   checkEquals(similarity2, 1)
#   similarity3 <- cluster_similarity_adapt(labels1, labels3, similarity = "jaccard") # expect 1
#   checkEquals(similarity3, 1)
#   similarity4 <- cluster_similarity_adapt(labels1, labels4, similarity = "jaccard") # expect 0
#   checkEquals(similarity4, 0)
#
#   checkTrue(is(similarity1, "numeric")) # check result numeric
#   checkEquals(length(similarity1), 1) # check only 1 result
# }
