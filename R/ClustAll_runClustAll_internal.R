# runClustAll_internal functions definition ------------------------------------
# These functions are not meant to be invoked directly by the user.
# See the createClusteAll function instead.

# This function creates a list with FMB objects to store the results
createEmptyArrays <- function(nvariables, data_use) {
  summary_matrices <- lapply(seq_len(nvariables), function(i) {
    as_FBM(matrix(0, nrow(data_use), nrow(data_use)))
  })
  return(summary_matrices)
}


# This function generates the PCA
generatePCA_derived <- function(data, variability, maxvar) {
  pca_go <- PCA(data, graph = FALSE, ncp=min(c(maxvar,ncol(data))))
  variab <- pca_go$eig[,3]
  whichgo <- min(which(variab>50))
  varigo <- min(c(whichgo,maxvar))
  result <- pca_go$ind$coord[,c(seq_len(varigo))]

  return(result)
}


# This functions fills the missing values with imputed data
obtainImputationData <- function(imp, impgo) {
  impData <- complete(imp,impgo)
  impData[is.na(impData)] <- 0
  impData <- data.frame(apply(impData, 2, as.numeric))
  impData <- impData[,apply(impData,2,sd)>0]

  return(impData)
}

# This function reads the slot of process from the object
isProcessed <- function(Object) {
  return(Object@processed)
}


# This function reduces the dimension by grouping the input variables into a
# dendrogram and generating PCAs for the resulting groups of variables
obtainDataPCA <- function(impData, variables_clust,
                          possible_heights,
                          heights_cut) {
  treego <- cutree(variables_clust, h=possible_heights[heights_cut])
  groups_go <- unique(treego)
  variables_use <- base::names(treego[treego==groups_go[1]])

  if (length(variables_use)==1) {
    data_PCA <- impData[,variables_use]

  } else {
    data_PCA <- generatePCA_derived(data=impData[,variables_use],
                                    variability=50, maxvar=3)
  }

  if (length(groups_go) > 1) {
    for (h in seq(from=2, to=length(groups_go))) {

      variables_use <- base::names(treego[treego==groups_go[h]])
      if (length(variables_use) == 1) {
        data_PCA <- cbind(data_PCA,impData[,variables_use])

      } else {
        data_PCA <- cbind(data_PCA,
                          generatePCA_derived(data=impData[,variables_use],
                                              variability=50,
                                              maxvar=3))
      }
    }
  }

  return(data_PCA)
}


# This function applies the K-Medoids clustering method, considering
# Gower distance, and return the results (c)
cstats.table_PAM <- function(dist, k) {
  clust.assess <- c("cluster.number", "wb.ratio", "dunn", "avg.silwidth")
  output.stats <- matrix(ncol = length(clust.assess), nrow = k-1)

  for (i in seq(from=2, to=k)) {
    pam_fit <- cluster::pam(dist, diss = TRUE, k = i)
    pam_clust_num <- (pam_fit$clustering)

    output.stats[i-1,] <- unlist(cluster.stats(d = dist,
                                               clustering = pam_clust_num)[clust.assess])
  }

  colnames(output.stats) <- clust.assess
  rownames(output.stats) <- c(seq(from=2, to=k))

  #return(output.stats)
  output.stats.df <- as.data.frame(output.stats)

  resultado <- median(c(output.stats.df[which.max(output.stats.df$avg.silwidth),]$cluster.number,
                        output.stats.df[which.max(output.stats.df$dunn),]$cluster.number,
                        output.stats.df[which.min(output.stats.df$wb.ratio),]$cluster.number))

  return(resultado)
}


# This function function applies the H-clust clustering method,
# considering Gower distance, and return the results (d)
cstats.table_hclust <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","wb.ratio","dunn","avg.silwidth")
  output.stats <- matrix(ncol = length(clust.assess), nrow = k-1)

  result_list <- lapply(seq(from = 2, to = k), function(i) {
    unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])
  })

  output.stats <- do.call(rbind, result_list)

  colnames(output.stats) <- clust.assess
  rownames(output.stats) <- seq(from = 2, to = k)

  output.stats.df <- as.data.frame(output.stats)

  result <- median(c(output.stats.df[which.max(output.stats.df$avg.silwidth),]$cluster.number,
                     output.stats.df[which.max(output.stats.df$dunn),]$cluster.number,
                     output.stats.df[which.min(output.stats.df$wb.ratio),]$cluster.number))

  return(result)
}



# This function filters the summary clusters with 0s and joins the four
# of the into a vector
obtainSummaryCluster <- function(summary_clusters_a, summary_clusters_b,
                                 summary_clusters_c, summary_clusters_d) {
  # remove those rows with only 0s
  nclust_a <- as.data.frame(summary_clusters_a[which(apply(summary_clusters_a[],
                                                           1, sum)>0),])
  nclust_b <- as.data.frame(summary_clusters_b[which(apply(summary_clusters_b[],
                                                           1, sum)>0),])
  nclust_c <- as.data.frame(summary_clusters_c[which(apply(summary_clusters_c[],
                                                           1, sum)>0),])
  nclust_d <- as.data.frame(summary_clusters_d[which(apply(summary_clusters_d[],
                                                           1, sum)>0),])

  # put all the clusters together
  summary_n_clust <- c(apply(nclust_a, 1, function(x) median(x, na.rm=TRUE)),
                       apply(nclust_b, 1, function(x) median(x, na.rm=TRUE)),
                       apply(nclust_c, 1, function(x) median(x, na.rm=TRUE)),
                       apply(nclust_d, 1, function(x) median(x, na.rm=TRUE)))

  return(summary_n_clust)
}


# This function prints the  ClustAll logo
printLogo <- function() {
  message("      ______ __              __   ___     __     __    ")
  message("     / ____// /__  __ _____ / /_ /   |   / /    / /    ")
  message("    / /    / // / / // ___// __// /| |  / /    / /     ")
  message("   / /___ / // /_/ /(__  )/ /_ / ___ | / /___ / /___   ")
  message("  /_____//_/ |__,_//____/ |__//_/  |_|/_____//_____/   ")
}


# The following code has been adapted from the clusteval v0.1 package created
# by John A. Ramey. https://cran.r-project.org/src/contrib/Archive/clusteval
# Also the rcpp_comembership and rcpp_comembership functions in C have been
# adapted to R
cluster_similarity_adapt <- function(labels1, labels2,
                                     similarity = c("jaccard", "rand"),
                                     method = "independence") {
  similarity <- match.arg(similarity)
  method <- match.arg(method)

  # Currently, we ignore the `method` argument and only use the similarity
  # statistics derived under an independence assumption
  switch(similarity,
         jaccard = jaccard_indep(labels1, labels2),
         rand = rand_indep(labels1, labels2))
}


jaccard_indep <- function(labels1, labels2) {
  com_table <- comembership_table(labels1, labels2)
  jaccard_out <- with(com_table, n_11 / (n_11 + n_10 + n_01))

  # In the case where 'labels1' and 'labels2' contain all singletons,the Jaccard
  # coefficient results in the expression 0 / 0, which yields a NaN value in R
  # We define such cases as 0
  if (is.nan(jaccard_out)) {
    warning("The two clusterings contain all singletons -- returning 0.")
    jaccard_out <- 0
  }
  jaccard_out
}


rand_indep <- function(labels1, labels2) {
  com_table <- comembership_table(labels1, labels2)
  with(com_table, (n_11 + n_00) / (n_11 + n_10 + n_01 + n_00))
}


#' @references Tibshirani, R. and  Walther, G. (2005). Cluster Validation by
#' Prediction Strength. Journal of Computational and Graphical Statistics, 14,3,
#' 511-528. \url{http://amstat.tandfonline.com/doi/abs/10.1198/106186005X59243}
comembership <- function(labels) {
  n <- length(labels)

  # The comembership vector is of length "n choose 2"
  comembership <- numeric(n * (n - 1) / 2)

  # The comembership index
  idx_comembership <- 1

  # Traverse through all pairs of observations to identify comemberships
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (labels[i] == labels[j]) {
        comembership[idx_comembership] <- 1
      }
      idx_comembership <- idx_comembership + 1
    }
  }

  return(comembership)
}

comembership_table <- function(labels1, labels2) {
  n1 <- length(labels1)
  n2 <- length(labels2)

  if (n1 != n2) {
    stop("The two vectors of cluster labels must be of equal length.")
  }

  # The counts of comembership pairs
  # n_11: the number of comemberships in both partitions
  # n_10: the number of comemberships in clustering 1 but not in clustering 2
  # n_01: the number of comemberships in clustering 2 but not in clustering 1
  # n_00: the number of non-comemberships in both partitions
  n_11 <- 0
  n_10 <- 0
  n_01 <- 0
  n_00 <- 0

  # Flags that indicate if the current pair of cluster labels in clusterings 1
  # and 2 are comemberships
  comembership1 <- FALSE
  comembership2 <- FALSE

  # Traverse through all pairs of observations to identify comemberships
  for (i in 1:(n1 - 1)) {
    for (j in (i + 1):n1) {
      # If either of the clusterings have a comembership, set the comembership
      # flag as true.
      comembership1 <- labels1[i] == labels1[j]
      comembership2 <- labels2[i] == labels2[j]

      if (comembership1 && comembership2) {
        n_11 <- n_11 + 1
      } else if (comembership1 && !comembership2) {
        n_10 <- n_10 + 1
      } else if (!comembership1 && comembership2) {
        n_01 <- n_01 + 1
      } else {  # if (!comembership1 && !comembership2)
        n_00 <- n_00 + 1
      }
    }
  }

  # Returns a list that contains the 2x2 contingency table results
  return(list(n_11 = n_11, n_10 = n_10, n_01 = n_01, n_00 = n_00))
}


# END OF ClustAll_runClustAll_internal.R

