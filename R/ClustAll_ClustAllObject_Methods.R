# ClustAllObject_Methods--------------------------------------------------------
#' @import ComplexHeatmap
#' @import circlize
#' @import RColorBrewer
#' @import ggplot2
#' @import grid
#' @import grDevices
#' @title Visualize Jaccard Distances Between Robust Stratifications
#' @aliases plotJACCARD,ClustAllObject,logicalOrNA,numericOrNA-method
#' @description
#' This function generates a heatmap visualization of the Jaccard distances
#' between robust stratifications identified by the ClustALL algorithm. It
#' provides a visual representation of the similarity between different
#' clustering solutions, helping to identify groups of related stratifications.
#'
#' @usage plotJACCARD(Object, paint = TRUE, stratification_similarity = 0.7)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} before using this function.
#' @param paint Logical. If TRUE (default), the function highlights groups of
#' similar stratifications on the heatmap with red squares. This helps in
#' visually identifying clusters of similar stratifications.
#' @param stratification_similarity Numeric value between 0 and 1. Sets the
#' threshold Jaccard distance for considering two stratifications as similar.
#' Default is 0.7. Higher values result in more stringent similarity criteria.
#'
#' @return A plot displaying a correlation matrix heatmap that shows the Jaccard
#' Distances between population-based robust stratifications. The heatmap
#' visually distinguishes groups of similar stratifications according to the
#' specified “stratification_similarity” threshold.
#'
#' @details
#' The plotJACCARD function visualizes the similarity between robust
#' stratifications using a heatmap of Jaccard distances:
#'
#' \itemize{
#'   \item The heatmap color scale represents Jaccard distances, with darker colors
#'     indicating higher similarity (lower distance).
#'   \item Stratifications are ordered based on hierarchical clustering of their
#'     Jaccard distances.
#'   \item The 'paint' option highlights groups of similar stratifications, making it
#'     easier to identify clusters of related solutions.
#'   \item The 'stratification_similarity' parameter allows fine-tuning of what is
#'     considered "similar" for the purpose of highlighting.
#' }
#'
#' The function provides annotations for each stratification, including:
#' \itemize{
#'   \item Distance metric used (e.g., Correlation, Gower)
#'   \item Clustering method employed (e.g., H-Clustering, K-Means, K-Medoids)
#'   \item Depth of the dendrogram cut used in the Data Complexity Reduction step
#' }
#'
#' This visualization is particularly useful for:
#' \itemize{
#'   \item Identifying groups of similar stratifications
#'   \item Assessing the overall diversity of robust solutions
#'   \item Guiding the selection of representative stratifications for further analysis
#' }
#'
#' @note
#' \itemize{
#'   \item This function requires a processed ClustAllObject.
#'     Ensure \code{\link{runClustAll}}
#'     has been executed before using plotJACCARD.
#'   \item The 'paint' feature may not be visible if there are no groups of stratifications
#'     meeting the similarity threshold.
#'   \item For exploring stratifications, it's recommended to start with a high
#'     'stratification_similarity' value and gradually decrease it to examine
#'     various levels of stratification grouping.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{resStratification}},
#' \code{\link{ClustAllObject-class}}, \code{\link{JACCARD_DISTANCE_F}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' plotJACCARD(obj_noNA1, paint = TRUE, stratification_similarity = 0.9)
#' }
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE, stratification_similarity=0.7)
  {standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject",
    paint="logicalOrNA",
    stratification_similarity="numericOrNA"),
  definition=function(Object, paint=TRUE, stratification_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order
    m <- Object@JACCARD_DISTANCE_F[ordergo, ordergo]
    robust_stratification <- obtain_metadata(m)
    res <- resStratification(Object, population=1e-9, all=TRUE,
                             stratification_similarity)
    full_length <- ncol(Object@JACCARD_DISTANCE_F)

    # Correlation matrix heatmap with the robust stratifications
    col_fun <- colorRamp2(c(0, 1),
                          colors = grDevices::colorRampPalette(brewer.pal(9,"Blues"))(25),
                          breaks=seq(0, 1, length.out = 25))

    legend <- HeatmapAnnotation(JACCARD_index = seq(0, 1,
                                length.out = ncol(m)),
                                col = list(lg = col_fun),
                                annotation_name_side = "right")
    ra <- rowAnnotation(
            Distance=robust_stratification[,"Distance"],
            Clustering=robust_stratification[,"Clustering"],
            Depth=robust_stratification[,"Depth"],
            col = list(Distance=structure(names=c("Correlation","Gower"),
                                          c("#CCCCFF","blue4") ),
                       Clustering=structure(names=c("H-Clustering", "K-Means",
                                                    "K-Medoids"),
                                            c("forestgreen", "sandybrown",
                                              "tomato3")),
                       Depth=colorRamp2(c(1,
                                          round(max(robust_stratification[, "Depth"])/2),
                                          max(robust_stratification[, "Depth"])),
                                        c("darkcyan", "#F7DCCA", "#C75F97"))))

    hp <- Heatmap(as.matrix(m), name="hp", cluster_columns=FALSE,
                  cluster_rows=FALSE, left_annotation=ra, col=col_fun,
                  bottom_annotation = NULL, show_column_names = FALSE,
                  heatmap_legend_param = list(
                    direction = "vertical", title="JACCARD index", at=c(0, 1),
                    legend_width = unit(10, "cm")
                  ))

    if (is.null(res)) {
      paint <- FALSE
    }

    if (paint == TRUE) {
      ht_opt$message <- FALSE
      draw(hp)
      ngroup <- 0
      paint_names <- unlist(lapply(res, function(sublist) {
        c(sublist[[1]][[1]], sublist[[length(sublist)]][[1]])
      }))

      invisible(lapply(seq(from = 1, to = length(paint_names), by = 2), function(z) {
              ngroup <- ngroup + 1
              index <- which(rownames(m) %in% c(paint_names[z], paint_names[z + 1]))
              start <- index[1] - 1
              finish <- index[2]
              decorate_heatmap_body("hp", row_slice = 1, column_slice = 1, {
                grid::grid.rect(unit(start / full_length, "npc"), unit(1 - start / full_length, "npc"), # top left
                                width = (finish - start) / full_length,
                                height = (finish - start) / full_length,
                                gp = gpar(lwd = 2.5, lty = 2.5, fill = FALSE, col = "red"),
                                just = c("left", "top"), draw = TRUE
                )
              })
            }))

    } else {
      return(hp)
    }
  }
)


#' @title Extract Representative Stratifications from ClustAllObject
#' @aliases resStratification,ClustAllObject,numericOrNA,logicalOrNA,numericOrNA-method
#' @description
#' This function retrieves and filters representative stratifications from a
#' processed ClustAllObject. It allows users to explore the most robust and
#' significant clustering solutions generated by the ClustALL algorithm, based
#' on population size criteria and stratification similarity.
#'
#' @usage resStratification(Object, population = 0.05, all = FALSE,
#'                          stratification_similarity = 0.7)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} before using this function.
#'
#' @param population Numeric value between 0 and 1. Specifies the minimum
#' proportion of the total population that a cluster within a stratification must
#' contain to be considered valid. Default is 0.05.
#'
#' @param all A logical value. If set to TRUE, the function will return all
#' stratification representatives that meet the robustness criteria of each
#' group of similar stratifications. If set to FALSE, only the centroid
#' stratification (the central that serves as the representative stratification)
#' for each group of similar stratifications will be returned.
#'
#' @param stratification_similarity Numeric value between 0 and 1. Sets the
#' threshold Jaccard distance for considering two stratifications as similar.
#' Default is 0.7. Higher values result in more stringent similarity criteria.
#'
#' @return A list of representative stratifications and their associated clusters.
#' The structure of the return value depends on the 'all' parameter:
#' \itemize{
#'   \item If all = FALSE: A named list where each element represents the centroid
#'     stratification for a group of similar stratifications.
#'   \item If all = TRUE: A nested list where each top-level element represents a group
#'     of similar stratifications, and contains all stratifications in that group.
#' }
#' Each stratification is represented by a table showing the distribution of
#' patients across clusters.
#'
#' @details
#' The resStratification function performs several key steps:
#'
#' \enumerate{
#'   \item Filters stratifications based on the 'population' parameter, ensuring that
#'     each cluster in a stratification contains at least the specified proportion
#'     of the total population.
#'   \item Groups similar stratifications based on their Jaccard distances, using the
#'     'stratification_similarity' threshold.
#'   \item For each group of similar stratifications:
#'   \itemize{
#'     \item If all = FALSE, selects the centroid (most representative) stratification.
#'     \item If all = TRUE, includes all stratifications in the group.
#'   }
#' }
#'
#' This function is particularly useful for:
#' \itemize{
#'   \item Identifying the most robust and significant clustering solutions.
#'   \item Reducing the number of stratifications to a manageable set for further analysis.
#'   \item Exploring how different similarity thresholds affect the grouping of stratifications.
#'   \item Comparing multiple similar stratifications within each group (when all = TRUE).
#' }
#'
#' @note
#' \itemize{
#'   \item This function requires a processed ClustAllObject. Ensure
#'     \code{\link{runClustAll}}
#'     has been executed before using resStratification.
#'   \item The 'population' parameter helps filter out stratifications with very small,
#'     potentially insignificant clusters.
#'   \item The 'stratification_similarity' parameter allows for fine-tuning the balance
#'     between diversity and similarity in the returned stratifications.
#'   \item When exploring results, it's often useful to try different combinations of
#'     'population' and 'stratification_similarity' values to understand the
#'     characteristics of your clustering solutions.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{plotJACCARD}},
#' \code{\link{cluster2data}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' }
#'
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=0.05, all=FALSE,
               stratification_similarity=0.7)
  {standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA",
    stratification_similarity="numericOrNA"),
  definition=function(Object, population=0.05, all=FALSE,
                      stratification_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object has not been processed yet.")
      message(". You need to execute runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo,
                                                                      ordergo],
                                            stratification_similarity)

    if (length(definitive_clusters) > 0) {
      # choose the representative stratification considering at least a minimum
      # percentage of the total population in each cluster. Default is 0.05 (5%)
      res <- chooseClusters(definitive_clusters, Object@summary_clusters,
                            population, all)
      stratificationRep <- lapply(seq_along(res), function(i) {
        if (all == TRUE) {
          inner_list <- lapply(seq_along(res[[i]]), function(j) {
            list(res[[i]][j], table(Object@summary_clusters[[res[[i]][j]]]))
          })
          names(inner_list) <- paste0("Stratification_", seq_along(res[[i]]))
          inner_list
        } else {
          list(table(Object@summary_clusters[[res[[i]]]]))
        }
      })

      if (all == FALSE) {
        names(stratificationRep) <- res
      } else {
      names(stratificationRep) <- paste0("Cluster_",
                                         seq(length(stratificationRep)))
      }

      return(stratificationRep)

    } else {
      message("There are no robust groups of stratification for the selected parameters.")
      return(list(NULL))
    }
  }
)


#' @title Export Stratification Results with Original Data
#' @aliases cluster2data,ClustAllObject,character-method
#' @description
#' This function combines the original input data with one or more selected
#' stratifications from the ClustALL algorithm results. It allows users to
#' examine how samples are clustered in the context of their original features,
#' facilitating further analysis and interpretation of the stratification
#' results.
#'
#' @usage cluster2data(Object, stratificationName)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object.
#' The object must have been processed by \code{\link{runClustAll}} before using
#' this function.
#' @param stratificationName A character vector specifying the names of one or more
#' stratifications to be exported. These names should correspond to stratifications
#' generated by the ClustALL algorithm and stored in the Object.
#'
#' @return
#' A data.frame that includes the original data with additional column(s)
#' containing the selected stratification(s). Each selected stratification will
#' be added as a separate column to the original dataset.
#'
#' @details
#' The cluster2data function serves some important purposes in the ClustALL workflow:
#'
#' 1. Data Integration: It combines clustering results with the original feature data,
#'    allowing for comprehensive analysis of how cluster assignments relate to
#'    input variables.
#'
#' 2. Preparation for External Analysis: The resulting data frame can be easily
#'    exported for use in other analytical tools or visualization software.
#'
#' The function is particularly useful for:
#' \itemize{
#'   \item Identifying features that distinguish different clusters
#'   \item Comparing how samples are grouped across different stratifications
#'   \item Preparing data for cluster-specific statistical analyses
#'   \item Creating visualizations that incorporate both cluster assignments and original features
#' }
#'
#' @note
#' \itemize{
#'   \item This function requires a processed ClustAllObject. Ensure \code{\link{runClustAll}}
#'     has been executed before using cluster2data.
#'   \item The stratificationName parameter accepts multiple stratification names, allowing
#'     for simultaneous export of multiple clustering solutions.
#'   \item Stratification names can be obtained from the results of \code{\link{resStratification}}
#'     or by examining the names in the summary_clusters slot of the ClustAllObject.
#'   \item The original data in the returned data frame includes all preprocessing steps
#'     applied during the creation of the ClustAllObject, such as one-hot encoding
#'     of categorical variables.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{resStratification}},
#' \code{\link{ClustAllObject-class}}, \code{\link{createClustAll}}
#'
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' df <- cluster2data(Object = obj_noNA1,
#'                    stratificationName = c("cuts_a_1","cuts_b_5","cuts_a_5"))
#' }
#' @export
setGeneric(
  name="cluster2data",
  def=function(Object, stratificationName){standardGeneric("cluster2data")}
)

setMethod(
  f="cluster2data",
  signature=signature(
    Object="ClustAllObject",
    stratificationName="character"),
  definition=function(Object, stratificationName) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    checkCluster(stratificationName, Object@summary_clusters)

    df <- Object@dataOriginal
    df[, stratificationName] <- Object@summary_clusters[stratificationName]

    return(df)
  }
)


#' @import networkD3
#' @import dplyr
#' @title Visualize Stratification Comparisons with Sankey Diagram
#' @aliases plotSANKEY,ClustAllObject,character,logicalOrNA-method
#' @description
#' This function generates a Sankey diagram to visualize the relationships between
#' different stratifications or between a stratification and the true labels (if available).
#' It provides an intuitive representation of how samples are distributed across
#' clusters in different stratifications or how they align with known stratifications.
#'
#' @usage plotSANKEY(Object, clusters, validationData = FALSE)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} before using this function.
#' @param clusters A character vector specifying the names of stratifications to compare.
#' If validationData is FALSE, exactly two stratification names should be provided.
#' If validationData is TRUE, provide one stratification name to compare with true labels.
#' @param validationData Logical. If TRUE, compares the specified stratification with
#' the true labels (validation data) if available. Default is FALSE.
#'
#' @return A Sankey plot showing the composition and flow of clusters between
#' the selected stratifications. If true labels are available and
#' “validationData” is TRUE, the plot will compare the selected stratification
#' against the true labels.
#'
#' @details
#' The plotSANKEY function provides a powerful visualization tool for understanding
#' the relationships between different clustering solutions or between a clustering
#' solution and known classifications:
#'
#' \enumerate{
#'   \item Stratification Comparison (validationData = FALSE):
#'   \itemize{
#'     \item Visualizes how samples are distributed across clusters in two different stratifications.
#'     \item Helps identify similarities and differences between clustering solutions.
#'     \item Useful for understanding how changes in clustering parameters affect sample groupings.
#'   }
#'
#'   \item Validation Comparison (validationData = TRUE):
#'   \itemize{
#'     \item Compares a single stratification with true labels (if available).
#'     \item Helps assess how well the clustering aligns with known stratifications.
#'     \item Useful for evaluating the biological or clinical relevance of a clustering solution.
#'   }
#' }
#'
#' The Sankey diagram represents:
#' \itemize{
#'   \item Clusters (or true labels) as nodes on the left and right sides of the diagram.
#'   \item Flows between nodes indicating how samples are distributed.
#'   \item The width of each flow is proportional to the number of samples it represents.
#' }
#'
#' This visualization is particularly useful for:
#' \itemize{
#'   \item Identifying stable sample groupings across different stratifications.
#'   \item Detecting major shifts in cluster assignments between solutions.
#'   \item Evaluating the concordance between clustering results and known stratifications.
#'   \item Understanding the impact of different clustering approaches on sample groupings.
#' }
#'
#' @note
#' \itemize{
#'   \item This function requires a processed ClustAllObject. Ensure
#'     \code{\link{runClustAll}}
#'     has been executed before using plotSANKEY.
#'   \item When validationData is TRUE, the ClustAllObject must contain validation data
#'     (true labels). This can be added using \code{\link{addValidationData}} if not
#'     provided during object creation.
#'   \item Stratification names can be obtained from the results of
#'     \code{\link{resStratification}}
#'     or by examining the names in the summary_clusters slot of the ClustAllObject.
#'   \item The Sankey diagram may become cluttered if there are many clusters or if the
#'     clustering solutions are very different. In such cases, consider focusing on
#'     specific subsets of clusters or using additional filtering criteria.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{resStratification}},
#' \code{\link{addValidationData}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' label <- label[16:30]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_a_1","cuts_b_5"))
#'
#' obj_noNA1 <- addValidationData(obj_noNA1, label)
#' plotSANKEY(Object = obj_noNA1, clusters = "cuts_a_1", validationData=TRUE)
#' }
#'
#' @export
setGeneric(
  name="plotSANKEY",
  def=function(Object, clusters, validationData=FALSE)
  {standardGeneric("plotSANKEY")}
)

setMethod(
  f="plotSANKEY",
  signature=signature(
    Object="ClustAllObject",
    clusters="character",
    validationData="logicalOrNA"),
  definition=function(Object, clusters, validationData=FALSE) {
    if (isProcessed(Object) == FALSE) {
      message("The object has not been processed yet.")
      message("You need to execute runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    if (validationData) {
      if (is.null(Object@dataValidation)) {
        message("The ClustALL Object does not include labels.")
        message("Please create a new object or modidy the object with the original labelling data. \nFor that check addValidationData method.")
        stop()
      } else if (length(clusters) != 1) {
        message("More than one stratifications have been selected.")
        message("Only the first one will be considered to plot against the original labelling data.")
        clusters <- clusters[1]
      }
    } else if (length(clusters) != 2) {
      message("You need to include the name of a pair of stratifications.")
      message("To obtain the stratifications you may use resStratification.")
      stop()
    }

    checkCluster(clusters, Object@summary_clusters)

    df <- cluster2data(Object, clusters)

    if (validationData) {
      df[, "validation"] <- Object@dataValidation
      clusters <- c(clusters, "validation")
    }

    df <- df[, clusters]
    names1 <- paste0(clusters[1], "_Group_",
                     seq_len(length(unique(df[,clusters[1]]))))
    names2 <- paste0(clusters[2], "_Group_",
                     seq_len(length(unique(df[,clusters[2]]))))

    source <- sort(rep(names1, times=length(unique(df[,clusters[2]]))))
    target <- rep(names2, times=length(unique(df[,clusters[1]])))

    value <- unlist(lapply(unique(df[, 1]), function(i) {
      df_tmp <- df[df[, 1] == i, ]
      tmp <- nrow(df)
      unlist(lapply(unique(df[, 2]), function(j) {
        length(which(df_tmp[, 2] == j)) / tmp
      }))
    }))

    links <- data.frame(source = source,
                        target = target,
                        value = value)

    nodes <- data.frame(name=c(as.character(links$source),
                               as.character(links$target)) %>% unique()
    )

    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    plot <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                     Source = "IDsource", Target = "IDtarget",
                                     Value = "value", NodeID = "name",
                                     sinksRight=FALSE)

    return(plot)
  }
)


#' @title Validate Stratification Results Against True Labels
#' @aliases validateStratification,ClustAllObject,characterOrNA-method
#' @description
#' This function calculates the sensitivity and specificity of a selected
#' stratification by comparing it to the true labels (validation data) stored
#' in the ClustAllObject. It provides a quantitative assessment of how well
#' the clustering aligns with known classifications.
#'
#' @usage validateStratification(Object, stratificationName)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} and contain validation
#' data (true labels).
#' @param stratificationName A character string specifying the name of the
#' stratification to be validated. This should correspond to a stratification
#' generated by the ClustALL algorithm and stored in the Object.
#'
#' @return A named numeric vector containing two elements:
#' \itemize{
#'   \item sensitivity: The proportion of true positive classifications
#'   \item specificity: The proportion of true negative classifications
#' }
#'
#' @details
#' The validateStratification function provides a crucial step in assessing the
#' biological or clinical relevance of clustering results:
#'
#' \enumerate{
#'   \item Comparison Mechanism:
#'   \itemize{
#'     \item The function compares the cluster assignments of the selected stratification
#'       with the true labels provided in the validation data.
#'     \item It treats the problem as a binary classification task, considering one
#'       class as the "positive" class and all others as "negative".
#'   }
#'
#'   \item Sensitivity (True Positive Rate):
#'   \itemize{
#'     \item Measures the proportion of actual positive cases that were correctly identified.
#'     \item Calculated as: (True Positives) / (True Positives + False Negatives)
#'   }
#'
#'   \item Specificity (True Negative Rate):
#'   \itemize{
#'     \item Measures the proportion of actual negative cases that were correctly identified.
#'     \item Calculated as: (True Negatives) / (True Negatives + False Positives)
#'   }
#'
#'   \item Interpretation:
#'   \itemize{
#'     \item Higher sensitivity indicates better identification of the positive class.
#'     \item Higher specificity indicates better identification of the negative classes.
#'     \item The function automatically adjusts calculations if necessary to ensure
#'       sensitivity and specificity are always higher than 0.5.
#'   }
#' }
#'
#' This function is particularly useful for:
#' \itemize{
#'   \item Evaluating the clinical or biological relevance of clustering solutions
#'   \item Comparing different stratifications based on their alignment with known classifications
#'   \item Identifying stratifications that best capture known groupings in the data
#'   \item Providing quantitative metrics to support the selection of optimal clustering solutions
#' }
#'
#' @note
#' \itemize{
#'   \item This function requires a processed ClustAllObject with validation data.
#'     Ensure \code{\link{runClustAll}} has been executed and validation data
#'     has been added using \code{\link{addValidationData}} if not provided during
#'     object creation.
#'   \item The function assumes binary classification. For multi-class problems, it
#'     effectively treats one class as "positive" and all others as "negative".
#'   \item Stratification names can be obtained from the results of \code{\link{resStratification}}
#'     or by examining the names in the summary_clusters slot of the ClustAllObject.
#'   \item The function will stop with an error if the ClustAllObject does not contain
#'     validation data or if the specified stratification name is not found.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{resStratification}},
#' \code{\link{addValidationData}}, \code{\link{plotSANKEY}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' label <- label[16:30]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' obj_noNA1 <- addValidationData(Object = obj_noNA1,
#'                                dataValidation = label)
#' validateStratification(obj_noNA1, "cuts_a_1")
#' }
#'
#' @export
setGeneric(
  name="validateStratification",
  def=function(Object, stratificationName){
    standardGeneric("validateStratification")}
)

setMethod(
  f="validateStratification",
  signature=signature(
    Object="ClustAllObject",
    stratificationName="characterOrNA"),
  definition=function(Object, stratificationName) {

    if (is.null(Object@dataValidation)) {
      message("The ClustALL Object does not include labels.")
      message("Please create a new object or modify the object with the original labelling data. \nFor that check addValidationData method")
      stop()
    }

    if (isProcessed(Object) == FALSE) {
      message("The object has not been processed yet.")
      message("You need to execute runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    checkCluster(stratificationName, Object@summary_clusters)

    if (length(stratificationName) != 1) {
      message("More than one stratifications have been selected.")
      message("Only the first one will be considered.")
      stratificationName <- stratificationName[1]
    }

    df <- cluster2data(Object, stratificationName)
    res <- table(df[,stratificationName], Object@dataValidation)
    sensitivity <- res[3]/(res[3]+res[4])
    specifity <- res[2]/(res[2]+res[1])

    if (sensitivity < 0.5 & specifity < 0.5)
    {
      sensitivity <- 1 - sensitivity
      specifity <- 1 - specifity
    }

    showRes <- c(sensitivity, specifity)
    base::names(showRes) <- c("sensitivity", "specificity")

    return(showRes)
  }
)

# END OF ClustAll_ClustAllObject_Methods.R
