# runClustAll ------------------------------------------------------------------
#' @import clValid
#' @import mice
#' @import bigstatsr
#' @import FactoMineR
#' @import modeest
#' @import dplyr
#' @import fpc
#' @import clValid
#' @import doSNOW
#' @import foreach
#' @import flock
#' @import cluster
#' @import utils
#' @importFrom grDevices colorRampPalette
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom methods is new validObject
#' @importFrom stats as.dist cor cutree hclust median quantile sd
#' @title ClustAll: Data driven strategy to find hidden subgroups of patients within
#' complex diseases using clinical data
#' @aliases runClustAll,ClustAllObject,numericOrNA,logicalOrNA-method
#' @description This method runs the ClustAll pipeline
#'
#'
#' @usage runClustAll(Object,
#'                    threads=1,
#'                    simplify=FALSE)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param threads Numeric vector that indicates the number of cores to use
#' @param simplify if TRUE computes one out of four depths of the dendrogram
#'
#' @return An object of class \code{\link{ClustAllObject-class}}
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},
#' \code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' @export

setGeneric(
  name="runClustAll",
  def=function(Object, threads=1, simplify=FALSE) {
    standardGeneric("runClustAll")
  }
)


setMethod(
  f="runClustAll",
  signature=signature(
    Object="ClustAllObject",
    threads="numericOrNA",
    simplify="logicalOrNA"),
  definition=function(Object, threads=1, simplify=FALSE) {

    if (Object@processed == TRUE) {
      message("ClustAll pipeline have been already executed")
      message("Stopping the process...")
      message("You may want to use resStratification to explore the results.")
      stop()
    }

    packages.parallel <- c("clValid", "mice", "bigstatsr", "FactoMineR",
                           "modeest", "dplyr","fpc")
    functions.parallel <- c("obtainImputationData", "obtainDataPCA",
                            "cstats.table_PAM", "cstats.table_hclust",
                            "generatePCA_derived")
    nvariables <- ncol(Object@data)

    if (Object@nImputation == 0) {
      nRow <- 1
    } else {
      nRow <- Object@nImputation
    }

    # create empty arrays
    summary_clusters_a <- as_FBM(matrix(0, ncol(Object@data), nRow))
    summary_clusters_b <- as_FBM(matrix(0, ncol(Object@data), nRow))
    summary_clusters_c <- as_FBM(matrix(0, ncol(Object@data), nRow))
    summary_clusters_d <- as_FBM(matrix(0, ncol(Object@data), nRow))

    summary_matrices_a <- createEmptyArrays(ncol(Object@data), Object@data)
    summary_matrices_b <- createEmptyArrays(ncol(Object@data), Object@data)
    summary_matrices_c <- createEmptyArrays(ncol(Object@data), Object@data)
    summary_matrices_d <- createEmptyArrays(ncol(Object@data), Object@data)

    # set parallelization parameters
    lock <- tempfile() # synchronization between R processes

    if (Object@nImputation == 0) {
      nimp <- 1
    } else {
      nimp <- Object@nImputation
    }

    if (simplify == TRUE) {
      message("CAUTION! The simplify parameter is set to TRUE.")
      message("For more robust results consider changing the simplify parameter to FALSE.")
      simplifyNumber <- 4
    } else {
      simplifyNumber <- 1
    }

    if (threads > detectCores()) {
      message("A greater number of cores than detected has been included")
      message("The maximum number of cores will be used.")
      threads <- detectCores()
    }

    printLogo()

    message("Running Data Complexity Reduction and Stratification Process.")
    message("This step may take some time...\n")

    if (threads == 1) {
      message("Only one core is being used.")
      message("Consider selecting more cores for process parallelization")
      message("\n")
    }

    cl <- makeCluster(threads)
    registerDoSNOW(cl)

    if (nimp == 1) { # conditional if only one or no imputation was applied
      impgo <- 1
      ## Step 1. Data Complexity Reduction
      if (Object@nImputation == 0) {
        impData <- Object@data
        impData <- apply(impData, 2, as.numeric)
      } else {
        impData <- obtainImputationData(Object@dataImputed, impgo)
      }

      ### Step 1.1. Compute the dendogram among inpute variables
      cor_est_var <- cor(impData, method="spearman") # obtain correlations

      variables_clust <- hclust(as.dist(cor_est_var)) # hierarchical
      possible_heights <- variables_clust$height

      pb <- txtProgressBar(max=length(seq((length(possible_heights)-(ncol(Object@data)-2)),
                                          (length(possible_heights)-1), simplifyNumber)), style=3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      ### Step 1.2. Reprocessing
      # compute for all depths
      foreach(heights_cut=seq((length(possible_heights)-(ncol(Object@data)-2)),
                              (length(possible_heights)-1), simplifyNumber),
              .packages=packages.parallel, .export=functions.parallel,
              .options.snow=opts) %dopar% {

                data_PCA <- obtainDataPCA(impData, variables_clust, possible_heights, heights_cut)
                ## END OF Step 1. Data Complexity Reduction


                ## Step 2. Statrification Process
                ### Step 2.1. Conducting statrifications
                # scale PCA values for correlation distances: (a) K-MEANS and
                # (b) H-CLUST clustering
                data_PCA_scaled <- apply(data_PCA, 2, scale)
                # rename otherwise clValid function does not detect rownames
                rownames(data_PCA_scaled) <- seq(nrow(data_PCA_scaled))


                #### Step 2.1.1. a) K-Means method
                data_PCA.clValid_internal_kmeans <- clValid(data_PCA_scaled,
                                                            seq(from=2,to=6),
                                                            clMethods=c("kmeans"),
                                                            validation="internal",
                                                            maxitems=nrow(data_PCA_scaled),
                                                            metric="correlation")
                oS_kmeans <- as.numeric(mlv(optimalScores(data_PCA.clValid_internal_kmeans)[,3],
                                            method = "mfv"))

                if (length(oS_kmeans) > 1) {
                  # Choose the consensus (best k) after evaluating the
                  # cluster number by different metrics.
                  oS_kmeans <- median(oS_kmeans)
                }

                kmeans_res <- clusters(data_PCA.clValid_internal_kmeans, "kmeans")
                kmeans_res_c <- kmeans_res[as.character(oS_kmeans)][[1]]$cluster

                for (t in seq_len(as.numeric(oS_kmeans))) { # for every k
                  induse <- as.numeric(names(kmeans_res_c[kmeans_res_c==t]))

                  locked <- flock::lock(lock) # synchronization between processes
                  summary_matrices_a[[heights_cut]][induse, induse] <- summary_matrices_a[[heights_cut]][induse, induse] + 1
                  flock::unlock(locked)
                }

                summary_clusters_a[heights_cut, impgo] <- oS_kmeans


                #### Step 2.1.2. b) Correlation Distance and H-Clust clustering method
                data_PCA.clValid_internal_hclust <- clValid(data_PCA_scaled, seq(from=2, to=6),
                                                            clMethods=c("hierarchical"),
                                                            validation="internal",
                                                            maxitems=nrow(data_PCA_scaled),
                                                            metric="correlation")
                oS_hclust <- as.numeric(mlv(optimalScores(data_PCA.clValid_internal_hclust)[,3], method = "mfv"))

                if (length(oS_hclust) > 1) {
                  oS_hclust <- median(oS_hclust) # choose the median
                }

                hclust_res <- clusters(data_PCA.clValid_internal_hclust,
                                       "hierarchical")
                hclust_res_c <- cutree(hclust_res, k=oS_hclust)

                for (t in seq_len(oS_hclust)) {
                  induse <- as.numeric(names(hclust_res_c[hclust_res_c==t]))
                  locked <- flock::lock(lock) # synchronization between processes
                  summary_matrices_b[[heights_cut]][induse, induse] <- summary_matrices_b[[heights_cut]][induse, induse] + 1 # add result
                  flock::unlock(locked)
                }
                summary_clusters_b[heights_cut, impgo] <- oS_hclust


                # Gower distances: (c) K-Medoids clustering method and
                # (d) H-Clust clustering method
                PCA_gower <- as.data.frame(data_PCA)
                colnames(PCA_gower) <- c(seq_len(dim(PCA_gower)[2]))
                # those variables with binary values
                binary <- which(apply(PCA_gower,2,function(x) { all(x %in% seq(from=0, to=1)) }))

                if (length(binary)==1) {
                  PCA_gower[,binary] <- factor(PCA_gower[,binary])

                } else {
                  PCA_gower[,binary] <- data.frame(apply(PCA_gower[,binary],2,
                                                         as.logical))
                }

                PCA_gower <- PCA_gower %>% mutate_if(is.character, as.factor)
                gower_dist <- cluster::daisy(PCA_gower, metric="gower")


                #### Step 2.1.3. c) Gower Distance and K-Medoids clustering method.
                # Apply the previous function to get the optimal number of clusters
                # for each depth of the dendrogram, considering Gower Distance and
                # K-Medoids clustering method
                summary_clusters_c[heights_cut,impgo] <- cstats.table_PAM(dist=gower_dist,
                                                                          k=6)
                pam_res <- cluster::pam(gower_dist, k=summary_clusters_c[heights_cut,impgo])
                pam_res_c <- pam_res$clustering

                # Compute the stratification for each depth of the dendrogram,
                # each imputation and its corresponding optimal number of clusters
                for (t in seq_len(summary_clusters_c[heights_cut, impgo])) {
                  induse <- as.numeric(names(pam_res_c[pam_res_c==t]))

                  locked <- flock::lock(lock) # synchronization between processes
                  summary_matrices_c[[heights_cut]][induse, induse] <- summary_matrices_c[[heights_cut]][induse, induse] + 1
                  flock::unlock(locked)
                }


                #### Step 2.1.4. d) Gower Distance and H-Clust clustering method
                divisive.clust <- cluster::diana(as.matrix(gower_dist),
                                                 diss = TRUE, keep.diss = TRUE)
                summary_clusters_d[heights_cut,impgo] <- cstats.table_hclust(gower_dist,
                                                                             divisive.clust, 6)
                hclustgow_res_c <- cutree(divisive.clust,
                                          k=summary_clusters_d[heights_cut,impgo])
                names(hclustgow_res_c) <- seq_len(dim(Object@data)[1])

                for (t in seq_len(summary_clusters_d[heights_cut, impgo]))  {
                  induse <- as.numeric(names(hclustgow_res_c[hclustgow_res_c==t]))
                  locked <- flock::lock(lock) # synchronization between processes
                  summary_matrices_d[[heights_cut]][induse, induse] <- summary_matrices_d[[heights_cut]][induse, induse] + 1
                  flock::unlock(locked)
                }
              }

      ### END OF step, if only one or no imputation were considered
    } else { # if more than one imputation was considered

      pb <- txtProgressBar(max=nimp, style=3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress=progress)

      ## Step 1. Data Complexity Reduction
      foreach(impgo=seq_len(nimp), .packages=packages.parallel, .export=functions.parallel, .options.snow=opts) %dopar% { # for every imputation

        if (Object@nImputation == 0) {
          impData <- Object@data
          impData <- apply(impData, 2, as.numeric)
        } else {
          impData <- obtainImputationData(Object@dataImputed, impgo)
        }

        ### Step 1.1. Compute the dendrogram among input variables
        # obtain correlations between input variables
        cor_est_var <- cor(impData, method="spearman")

        # Compute the dendrogram among input variables
        variables_clust <- hclust(as.dist(cor_est_var))
        possible_heights <- variables_clust$height


        ### Step 1.2. Reprocessing to obtain the embeddings
        # compute for all possible heights
        for (heights_cut in seq((length(possible_heights)-(ncol(Object@data)-2)),
                                (length(possible_heights)-1), simplifyNumber)) {

          data_PCA <- obtainDataPCA(impData, variables_clust, possible_heights,
                                    heights_cut)
          ## END OF Step 1. Data Complexity Reduction


          ## Step 2. Statrification Process
          ### Step 2.1. Conducting statrifications
          # scale PCA values for correlation distances:
          # (a) K-MEANS and (b) H-CLUST clustering
          data_PCA_scaled <- apply(data_PCA, 2, scale)
          # rename otherwise clValid function does not detect rownames
          rownames(data_PCA_scaled) <- seq_len(nrow(data_PCA_scaled))


          #### Step 2.1.1. a) Correlation Distance and K-Means clustering method
          data_PCA.clValid_internal_kmeans <- clValid(data_PCA_scaled, seq(from=2, to=6),
                                                      clMethods=c("kmeans"),
                                                      validation="internal",
                                                      maxitems=nrow(data_PCA_scaled),
                                                      metric="correlation")
          oS_kmeans <- as.numeric(mlv(optimalScores(data_PCA.clValid_internal_kmeans)[,3], method = "mfv"))

          if (length(oS_kmeans) > 1) {
            # Choose the consensus (best k) after evaluating
            # the cluster number by different metrics
            oS_kmeans <- median(oS_kmeans) # choose the median
          }

          kmeans_res <- clusters(data_PCA.clValid_internal_kmeans, "kmeans")
          kmeans_res_c <- kmeans_res[as.character(oS_kmeans)][[1]]$cluster

          for (t in seq_len(as.numeric(oS_kmeans))) { # for every k
            induse <- as.numeric(names(kmeans_res_c[kmeans_res_c==t]))

            locked <- flock::lock(lock) # synchronization between processes
            summary_matrices_a[[heights_cut]][induse, induse] <- summary_matrices_a[[heights_cut]][induse, induse] + 1
            flock::unlock(locked)
          }

          summary_clusters_a[heights_cut, impgo] <- oS_kmeans


          #### Step 2.1.2. b) Correlation Distance and H-Clust clustering method
          data_PCA.clValid_internal_hclust <- clValid(data_PCA_scaled, seq(from=2, to=6),
                                                      clMethods=c("hierarchical"),
                                                      validation="internal",
                                                      maxitems=nrow(data_PCA_scaled),
                                                      metric="correlation")
          oS_hclust <- as.numeric(mlv(optimalScores(data_PCA.clValid_internal_hclust)[,3], method = "mfv"))

          if (length(oS_hclust) > 1) {
            # Choose the consensus (best k) after evaluating the
            # cluster number by different metrics
            oS_hclust <- median(oS_hclust)
          }

          hclust_res <- clusters(data_PCA.clValid_internal_hclust, "hierarchical")
          hclust_res_c <- cutree(hclust_res, k=oS_hclust)

          for (t in seq_len(oS_hclust)) {
            induse <- as.numeric(names(hclust_res_c[hclust_res_c==t]))
            locked <- flock::lock(lock) # synchronization
            summary_matrices_b[[heights_cut]][induse, induse] <- summary_matrices_b[[heights_cut]][induse, induse] + 1 # add result
            flock::unlock(locked)
          }
          summary_clusters_b[heights_cut, impgo] <- oS_hclust


          # Gower distances: (c)K-Medoids clustering method and
          # (d) H-Clust clustering method
          PCA_gower <- as.data.frame(data_PCA)
          colnames(PCA_gower) <- c(seq_len(dim(PCA_gower)[2]))
          binary <- which(apply(PCA_gower,2,function(x) { all(x %in% seq(from=0, to=1)) })) # those variables with binary values

          if (length(binary)==1) {
            PCA_gower[,binary] <- factor(PCA_gower[,binary])

          } else {
            PCA_gower[,binary] <- data.frame(apply(PCA_gower[,binary], 2, as.logical))
          }

          PCA_gower <- PCA_gower %>% mutate_if(is.character, as.factor)
          gower_dist <- cluster::daisy(PCA_gower, metric="gower")


          #### Step 2.1.3. c) Gower Distance and K-Medoids clustering method.
          # Apply the previous function to get the optimal number of clusters
          # for each depth of the dendrogram, considering Gower Distance and
          # K-Medoids clustering method.
          summary_clusters_c[heights_cut,impgo] <- cstats.table_PAM(dist=gower_dist,
                                                                    k=6)
          pam_res <- cluster::pam(gower_dist, k=summary_clusters_c[heights_cut,impgo])
          pam_res_c <- pam_res$clustering

          # calculate TREE for each cut and each imputation and
          # its corresponding optimal K
          for (t in seq_len(summary_clusters_c[heights_cut, impgo])) {
            induse <- as.numeric(names(pam_res_c[pam_res_c==t]))
            locked <- flock::lock(lock) # synchronization
            summary_matrices_c[[heights_cut]][induse, induse] <- summary_matrices_c[[heights_cut]][induse, induse] + 1
            flock::unlock(locked)
          }


          #### Step 2.1.4. d) Gower Distance and H-Clust clustering method
          divisive.clust <- cluster::diana(as.matrix(gower_dist), diss = TRUE, keep.diss = TRUE)
          summary_clusters_d[heights_cut,impgo] <- cstats.table_hclust(gower_dist, divisive.clust, 6)
          hclustgow_res_c <- cutree(divisive.clust, k=summary_clusters_d[heights_cut,impgo])
          names(hclustgow_res_c) <- seq_len(dim(Object@data)[1])

          for (t in seq_len(summary_clusters_d[heights_cut, impgo]))  {
            induse <- as.numeric(names(hclustgow_res_c[hclustgow_res_c==t]))
            locked <- flock::lock(lock) # synchronization between processes
            summary_matrices_d[[heights_cut]][induse, induse] <- summary_matrices_d[[heights_cut]][induse, induse] + 1
            flock::unlock(locked)
          }
        }
      }
    }
    ### END OF step if  more than one imputation were applied

    stopCluster(cl) # end of parallelization
    ### END OF Step 2.1. Conducting stratifications

    if (Object@nImputation != 0) {
      # put all the clusters together
      summary_n_clust <- obtainSummaryCluster(summary_clusters_a,
                                              summary_clusters_b,
                                              summary_clusters_c,
                                              summary_clusters_d)
    } else { # remove 0s and put all together
      summary_n_clust <- c(summary_clusters_a[][summary_clusters_a[]!=0],
                           summary_clusters_b[][summary_clusters_b[]!=0],
                           summary_clusters_c[][summary_clusters_c[]!=0],
                           summary_clusters_d[][summary_clusters_d[]!=0])
    }

    # rename the stratification matrices
    names(summary_matrices_a) <- paste0("cuts_a_",
                                        seq_len(length(summary_matrices_a)),
                                        sep="")
    names(summary_matrices_b) <- paste0("cuts_b_",
                                        seq_len(length(summary_matrices_b)),
                                        sep="")
    names(summary_matrices_c) <- paste0("cuts_c_",
                                        seq_len(length(summary_matrices_c)),
                                        sep="")
    names(summary_matrices_d) <- paste0("cuts_d_",
                                        seq_len(length(summary_matrices_d)),
                                        sep="")

    summary_matrices_MEASURES <- c(summary_matrices_a, summary_matrices_b,
                                   summary_matrices_c, summary_matrices_d)

    # remove those matrices with only 0s
    tmp <- lapply(summary_matrices_MEASURES, function(x) as.matrix(x[]))
    if (length(which(lapply(tmp, sum)==0)) > 0) {
      summary_matrices_MEASURES <- summary_matrices_MEASURES[-which(lapply(summary_matrices_MEASURES, function(x) sum(x[]))==0)]
    }


    ### Step 2.2. Filtering non-robust stratifications
    summary_clusters <- vector("list", length(summary_matrices_MEASURES))

    for(i in seq_len(length(summary_clusters))) {
      hclustgo <- hclust(1-as.dist(summary_matrices_MEASURES[[i]][]))
      summary_clusters[[i]] <- cutree(hclustgo, k=summary_n_clust[i])
    }
    names(summary_clusters) <- names(summary_matrices_MEASURES)

    # Jaccard Distance
    JACCARD_DISTANCE <- matrix(NA, length(summary_matrices_MEASURES),
                               length(summary_matrices_MEASURES))

    message("")
    message("\nCalculating correlation distance matrix of the statifications...\n")
    pb <- txtProgressBar(max=nimp, style=3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)

    # Register the parallel backend
    cl <- makeCluster(threads)
    registerDoSNOW(cl)

    # Create the matrix
    JACCARD_DISTANCE <- matrix(NA, length(summary_matrices_MEASURES), length(summary_matrices_MEASURES))
    rownames(JACCARD_DISTANCE) <- names(summary_matrices_MEASURES)
    colnames(JACCARD_DISTANCE) <- rownames(JACCARD_DISTANCE)

    # For two stratifications, this function calculates the similarity statistic
    # in comemberships of the observations.
    # The comembership is defined as the pairs of observations that are
    # clustered together.
    JACCARD_DISTANCE <- foreach(i = 1:nrow(JACCARD_DISTANCE), .combine = 'rbind', .options.snow=opts) %dopar% {
      result_row <- numeric(nrow(JACCARD_DISTANCE))
      for (j in i:nrow(JACCARD_DISTANCE)) {
        result_row[j] <- cluster_similarity_adapt(summary_clusters[[i]],
                                                  summary_clusters[[j]],
                                                  similarity = "jaccard")
      }
      result_row
    }

    # Stop the parallel backend
    stopCluster(cl)

    # Convert the result to a matrix
    JACCARD_DISTANCE <- as.matrix(JACCARD_DISTANCE)

    # Copy the upper triangle to the lower triangle
    JACCARD_DISTANCE[lower.tri(JACCARD_DISTANCE)] <- t(JACCARD_DISTANCE)[lower.tri(JACCARD_DISTANCE)]

    # Optionally, set the diagonal to 1
    diag(JACCARD_DISTANCE) <- 1

    rownames(JACCARD_DISTANCE) <- names(summary_matrices_MEASURES)
    colnames(JACCARD_DISTANCE) <- rownames(JACCARD_DISTANCE)

    message("")
    message("\nFiltering non-robust statifications...\n")

    pb <- txtProgressBar(min = 0, max = length(summary_matrices_MEASURES),
                         initial = 0, style = 3)

    # Population based-robustness: Bootstrapping
    summary_matrices_STABILITY <- matrix(NA,
                                         length(summary_matrices_MEASURES), 3)
    for(i in seq_len(length(summary_matrices_MEASURES))) {
      invisible(capture.output( # avoid printing function messages
        r1 <- clusterboot(data=as.dist(1000-summary_matrices_MEASURES[[i]][]),
                          B=100, distances=TRUE, bootmethod="boot",
                          bscompare=TRUE, multipleboot=FALSE,
                          jittertuning=0.05, noisetuning=c(0.05,4),
                          subtuning=floor(nrow(data)/2),
                          clustermethod=disthclustCBI,noisemethod=FALSE,
                          count=TRUE, showplots=FALSE,dissolution=0.5,
                          recover=0.75,method="complete",k=2)
      ))

      summary_matrices_STABILITY[i,seq(from=1,to=2)] <- as.numeric(r1$bootmean[seq(from=1,to=2)])
      summary_matrices_STABILITY[i,3] <- mean(summary_matrices_STABILITY[i,seq(from=1,to=2)])

      setTxtProgressBar(pb,i)
    }

    quantileuse <- 0.85 # Stratifications with less than 85% stability are excluded
    qgo <- quantile(summary_matrices_STABILITY[,3], quantileuse)
    JACCARD_DISTANCE_F <- JACCARD_DISTANCE[summary_matrices_STABILITY[, 3] >= qgo,
                                           summary_matrices_STABILITY[, 3]>=qgo]
    ### END OF Step 2.2. Filtering non-robust stratifications

    Object@summary_clusters <- summary_clusters
    Object@JACCARD_DISTANCE_F <- JACCARD_DISTANCE_F
    Object@processed <- TRUE

    message("\nClustAll pipeline finished successfully!\n")

    return(Object)
  }
)
# END OF ClustAll_runClustAll.R
