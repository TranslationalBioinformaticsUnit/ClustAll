# ClustAllObject_Methods--------------------------------------------------------
#' @import ComplexHeatmap
#' @import circlize
#' @import RColorBrewer
#' @import ggplot2
#' @import grid
#' @import grDevices
#' @title Correlation matrix heatmap showing the Jaccard distance between
#' robust stratifications in the ClustAllObject
#' @aliases plotJACCARD,ClustAllObject,logicalOrNA,numericOrNA-method
#' @description
#' This function plots the correlation matrix heatmap showing the Jaccard
#' Distance between robust stratifications
#' @usage plotJACCARD(Object,
#'                    paint=TRUE,
#'                    stratification_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param paint Logical vector with the annotation for the different
#' stratifications
#' @param stratification_similarity The minimum Jaccard Distance value to
#' consider two stratifications similar. Default is 0.7.
#' @return plot
#'
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
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
                       Clustering=structure(names=c("Hierarchical", "k-means",
                                                    "k-medoids"),
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
      draw(hp)
      ngroup <- 0
      paint_names <- c()
      for (i in seq_len(length(res))) {
        paint_names <- c(paint_names, res[[i]][[1]][1])
        paint_names <- c(paint_names, res[[i]][[length(res[[i]])]][1])
      }

      for (z in seq(from=1, to=length(paint_names), by=2)) {
        ngroup <- ngroup + 1
        index <- which(rownames(m) %in% c(paint_names[z], paint_names[z+1]))
        start <- index[1] - 1
        finish <- index[2]
        decorate_heatmap_body("hp", row_slice = 1, column_slice = 1, {
                        grid::grid.rect(unit(start/full_length, "npc"), unit(1-start/full_length,
                                                                       "npc"), # top left
                                  width = (finish-start)/full_length,
                                  height = (finish-start)/full_length,
                                  gp = gpar(lwd = 2.5, lty = 2.5, fill=FALSE, col="red"),
                                  just = c("left", "top"), draw = TRUE
          )
        })
      }
    } else {
      return(hp)
    }
  }
)


#' @title Show the stratification representatives from the ClustAllObject
#' @aliases resStratification,ClustAllObject,numericOrNA,logicalOrNA,numericOrNA-method
#' @description
#' This function returns the stratifications representatives by keeping those
#' clusters with a minimum percentage of the population. Default is 0.05.
#' It returns all the robust stratification (TRUE) or the representative for
#' each group of stratifications (FALSE). Default is FALSE
#' @usage resStratification(Object,
#'                          population=0.05,
#'                          all=FALSE,
#'                          stratification_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param population Numeric vector with the minimum percentage of the total
#' population that a stratification must have to be considered as representative
#' @param all Logical vector to return all the representative stratifications
#' per group of clusters. If it is FALSE, only the centroid stratification of
#' each group of clusters is returned
#' @param stratification_similarity The minimum Jaccard distance value to
#' consider two groups similar. Default is 0.7
#' @return list
#'
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
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
      message("Note that the number of cores to use can be specified")
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
          names(inner_list) <- paste("Cluster_", seq_along(res[[i]]))
          inner_list
        } else {
          list(table(Object@summary_clusters[[res[[i]]]]))
        }
      })

      if (all == FALSE) {
        names(stratificationRep) <- res
      }


      return(stratificationRep)

    } else {
      message("There are no robust groups of stratification for the selected parameters.")
      return(NULL)
    }
  }
)


#' @title cluster2data
#' @aliases cluster2data,ClustAllObject,character-method
#' @description
#' Returns the original data in a dataframe, including the selected robust
#' stratification(s) as varaibles. The representative stratification names can
#' be obtained using the method. \code{\link{resStratification}}
#' @usage cluster2data(Object,
#'                     stratificationName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param stratificationName Character vector with one or more stratification names
#'
#' @return data.frame
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
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
#' @title Plots Sankey Diagram showing the cluster distribution and shifts
#' between a pair of stratifications derived from ClustAllObject
#' @aliases plotSANKEY,ClustAllObject,character,logicalOrNA-method
#' @description
#' This function plots the Sankey Diagram with the cluster distribution and
#' shifts between a pair of stratifications
#' @usage plotSANKEY(Object,
#'                   clusters,
#'                   validationData=FALSE)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusters Character vector with the names of a pair of stratifications.
#' Check resStratification to obtain the stratification names.
#' @param validationData Logical value to use original labelling data to compare
#' with the ClustALL selected stratification.
#' @return plot
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},
#' \code{\link{ClustAllObject-class}}
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


#' @title validateStratification
#' @aliases validateStratification,ClustAllObject,characterOrNA-method
#' @description
#' Returns the sensitivity and specifity of the selected stratification the
#' original lebelling. The representative stratification names can be obtained
#' using the method \code{\link{resStratification}}
#' @usage validateStratification(Object,
#'                               stratificationName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param stratificationName Character vector with the name a stratification.
#' Check resStratification to obtain stratification names.
#'
#' @return numeric
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},
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
    showRes <- c(sensitivity, specifity)
    base::names(showRes) <- c("sensitivity", "specificity")

    return(showRes)
  }
)

# END OF ClustAll_ClustAllObject_Methods.R
