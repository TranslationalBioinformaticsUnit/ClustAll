# ClustAllObject_Methods_internal functions definition -------------------------
# These functions are not meant to be invoked directly by the user.
# See the ClustAllObject_Methods functions instead.

# This function returns a vector with the definitive stratifications based on
# the distance matrix
obtainDefCluster <- function(mat_using, cluster_similarity=0.7){
    candidates_clustering <- c()
    definitive_clusters <- list()
    j <- 1

    for (i in seq_len(ncol(mat_using)-1)) {
        # Avoid repeating same cluster
        if (i < j-1 | base::names(mat_using[i,])[j] %in% candidates_clustering) {
            next
        }

        if (i == ncol(mat_using)-1) {
            if (mat_using[i,i+1] >= cluster_similarity) {
                candidates_clustering <- c(base::names(mat_using[i,])[i],
                                           base::names(mat_using[i,])[i+1])
                definitive_clusters <- append(definitive_clusters,
                                                list(candidates_clustering))
            }
        } else {

            if (mat_using[i,i+1] >= cluster_similarity) {
                candidates_clustering <- c(base::names(mat_using[i,])[i])

                for (j in seq(from=(i+1), to=(ncol(mat_using)))) {

                    if (j==ncol(mat_using)) {
                        candidates_clustering <- c(candidates_clustering,
                                                   base::names(mat_using[i,])[j])
                        definitive_clusters <- append(definitive_clusters,
                                                    list(candidates_clustering))
                        break
                }

                if(mat_using[i,j] >= cluster_similarity) {
                    candidates_clustering <- c(unique(candidates_clustering),
                                               base::names(mat_using[i,])[j])
                    if (j == (ncol(mat_using)-2)) {
                        append(definitive_clusters, list(candidates_clustering))
                    }

            # The lower this value is, the larger group of clusters is obtained
                } else if (mat_using[i+1,j] >= cluster_similarity){
                    candidates_clustering <- c(unique(candidates_clustering),
                                               base::names(mat_using[i,])[j])
                    if (j == (ncol(mat_using)-2)) {
                        append(definitive_clusters, list(candidates_clustering))
                    }
                } else {
                    definitive_clusters <- append(definitive_clusters,
                                                    list(candidates_clustering))
                    break
                    }
                }
            }
        }
    }

    min_NperGroup <- 3
    definitive_clusters <- definitive_clusters[which(unlist(lapply(definitive_clusters,
                                               function(x) {length(x)>=min_NperGroup})))]

    return(definitive_clusters)
}


# Choose the representative stratification considering at least a minimum
# percentage of the total population in each cluster. Default is 0.05 (5%)
chooseClusters <- function(definitive_clusters, summary_clusters,
                           population = 0.05, all = TRUE) {
  if (0 > population | population >= 1) {
    message("Not a valid value. A value from 0 to 1 should be introduced.")
    message("Clusters contain at least 5% of the total population.")
    population <- 0.05
  }

  chosen_clusters <- list()

  possible_stratifications <- lapply(summary_clusters, function(cluster) {
    as.numeric(table(cluster))
  })

  definitive_clusters <- lapply(definitive_clusters, function(cluster) {
    for (j in cluster) {
      if (min(unlist(possible_stratifications[j])) < (length(summary_clusters[[1]])) * population) {
        cluster <- cluster[cluster != j]
      }
    }
    return(cluster)
  })

  chosen_clusters <- lapply(definitive_clusters, function(cluster) {
    if (length(cluster) == 0) {
      return(NULL)
    } else if ((length(cluster) %% 2) == 0) {
      return(cluster[length(cluster) / 2])
    } else {
      return(cluster[ceiling(length(cluster) / 2)])
    }
  })

  if (all == TRUE) {
    return(definitive_clusters)
  } else {
    return(chosen_clusters)
  }
}


# This function checks if the introduced cluster exists in the list of clusters
checkCluster <- function(cluster, lCluster) {
  stopProcess <- FALSE
  booleans <- cluster %in% base::names(lCluster)

  if(any(!booleans)) {
    message("The following cluster(s) do(es) not exist:")
    message(paste0(cluster[!booleans], sep="\n"))
    message("You may want to check the clusters using resStratification.")
    stopProcess <- TRUE
  }

  if (stopProcess){
    stop()
  }
}


# This function obtains the metadata fot the stratifications displayed in the
# correlation matrix heatmap with the Jaccard distance between robust
# stratifications
obtain_metadata <- function(m) {
    names <- rownames(m)
    split_names <- strsplit(names, "_")
    names_clustering <- sapply(split_names, function(x) x[2])
    names_depth <- sapply(split_names, function(x) as.numeric(x[3]))
    names_distance <- ifelse(names_clustering %in% c("a", "b"), "Correlation",
                                "Gower")
    names_clusteringmeth <- names_clustering
    names_clusteringmeth[which(names_clusteringmeth %in% c("b",
                                                        "d"))] <- "H-Clustering"
    names_clusteringmeth[which(names_clusteringmeth == "a")] <- "K-Means"
    names_clusteringmeth[which(names_clusteringmeth == "c")] <- "K-Medoids"

    return(data.frame("Distance"=names_distance,
                        "Clustering"=names_clusteringmeth,
                        "Depth"=names_depth))
}
# END OF ClustAll_ClustAllObject_Methods_internal.R
