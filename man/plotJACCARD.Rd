% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_ClustAllObject_Methods.R
\name{plotJACCARD}
\alias{plotJACCARD}
\alias{plotJACCARD,ClustAllObject,logicalOrNA,numericOrNA-method}
\title{Visualize Jaccard Distances Between Robust Stratifications}
\usage{
plotJACCARD(Object, paint = TRUE, stratification_similarity = 0.7)
}
\arguments{
\item{Object}{A processed \code{\link{ClustAllObject-class}} object. The object
must have been processed by \code{\link{runClustAll}} before using this function.}

\item{paint}{Logical. If TRUE (default), the function highlights groups of
similar stratifications on the heatmap with red squares. This helps in
visually identifying clusters of similar stratifications.}

\item{stratification_similarity}{Numeric value between 0 and 1. Sets the
threshold Jaccard distance for considering two stratifications as similar.
Default is 0.7. Higher values result in more stringent similarity criteria.}
}
\value{
A plot displaying a correlation matrix heatmap that shows the Jaccard
Distances between population-based robust stratifications. The heatmap
visually distinguishes groups of similar stratifications according to the
specified “stratification_similarity” threshold.
}
\description{
This function generates a heatmap visualization of the Jaccard distances
between robust stratifications identified by the ClustALL algorithm. It
provides a visual representation of the similarity between different
clustering solutions, helping to identify groups of related stratifications.
}
\details{
The plotJACCARD function visualizes the similarity between robust
stratifications using a heatmap of Jaccard distances:

\itemize{
  \item The heatmap color scale represents Jaccard distances, with darker colors
    indicating higher similarity (lower distance).
  \item Stratifications are ordered based on hierarchical clustering of their
    Jaccard distances.
  \item The 'paint' option highlights groups of similar stratifications, making it
    easier to identify clusters of related solutions.
  \item The 'stratification_similarity' parameter allows fine-tuning of what is
    considered "similar" for the purpose of highlighting.
}

The function provides annotations for each stratification, including:
\itemize{
  \item Distance metric used (e.g., Correlation, Gower)
  \item Clustering method employed (e.g., H-Clustering, K-Means, K-Medoids)
  \item Depth of the dendrogram cut used in the Data Complexity Reduction step
}

This visualization is particularly useful for:
\itemize{
  \item Identifying groups of similar stratifications
  \item Assessing the overall diversity of robust solutions
  \item Guiding the selection of representative stratifications for further analysis
}
}
\note{
\itemize{
  \item This function requires a processed ClustAllObject.
    Ensure \code{\link{runClustAll}}
    has been executed before using plotJACCARD.
  \item The 'paint' feature may not be visible if there are no groups of stratifications
    meeting the similarity threshold.
  \item For exploring stratifications, it's recommended to start with a high
    'stratification_similarity' value and gradually decrease it to examine
    various levels of stratification grouping.
}
}
\examples{
data("BreastCancerWisconsin", package = "ClustAll")
wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
wdbc <- wdbc[1:15,1:8]
obj_noNA <- createClustAll(data = wdbc)
\donttest{
obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
plotJACCARD(obj_noNA1, paint = TRUE, stratification_similarity = 0.9)
}
}
\seealso{
\code{\link{runClustAll}}, \code{\link{resStratification}},
\code{\link{ClustAllObject-class}}, \code{\link{JACCARD_DISTANCE_F}}
}
