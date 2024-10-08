% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_ClustAllObject_Methods.R
\name{resStratification}
\alias{resStratification}
\alias{resStratification,ClustAllObject,numericOrNA,logicalOrNA,numericOrNA-method}
\title{Extract Representative Stratifications from ClustAllObject}
\usage{
resStratification(Object, population = 0.05, all = FALSE,
                         stratification_similarity = 0.7)
}
\arguments{
\item{Object}{A processed \code{\link{ClustAllObject-class}} object. The object
must have been processed by \code{\link{runClustAll}} before using this function.}

\item{population}{Numeric value between 0 and 1. Specifies the minimum
proportion of the total population that a cluster within a stratification must
contain to be considered valid. Default is 0.05.}

\item{all}{A logical value. If set to TRUE, the function will return all
stratification representatives that meet the robustness criteria of each
group of similar stratifications. If set to FALSE, only the centroid
stratification (the central that serves as the representative stratification)
for each group of similar stratifications will be returned.}

\item{stratification_similarity}{Numeric value between 0 and 1. Sets the
threshold Jaccard distance for considering two stratifications as similar.
Default is 0.7. Higher values result in more stringent similarity criteria.}
}
\value{
A list of representative stratifications and their associated clusters.
The structure of the return value depends on the 'all' parameter:
\itemize{
  \item If all = FALSE: A named list where each element represents the centroid
    stratification for a group of similar stratifications.
  \item If all = TRUE: A nested list where each top-level element represents a group
    of similar stratifications, and contains all stratifications in that group.
}
Each stratification is represented by a table showing the distribution of
patients across clusters.
}
\description{
This function retrieves and filters representative stratifications from a
processed ClustAllObject. It allows users to explore the most robust and
significant clustering solutions generated by the ClustALL algorithm, based
on population size criteria and stratification similarity.
}
\details{
The resStratification function performs several key steps:

\enumerate{
  \item Filters stratifications based on the 'population' parameter, ensuring that
    each cluster in a stratification contains at least the specified proportion
    of the total population.
  \item Groups similar stratifications based on their Jaccard distances, using the
    'stratification_similarity' threshold.
  \item For each group of similar stratifications:
  \itemize{
    \item If all = FALSE, selects the centroid (most representative) stratification.
    \item If all = TRUE, includes all stratifications in the group.
  }
}

This function is particularly useful for:
\itemize{
  \item Identifying the most robust and significant clustering solutions.
  \item Reducing the number of stratifications to a manageable set for further analysis.
  \item Exploring how different similarity thresholds affect the grouping of stratifications.
  \item Comparing multiple similar stratifications within each group (when all = TRUE).
}
}
\note{
\itemize{
  \item This function requires a processed ClustAllObject. Ensure
    \code{\link{runClustAll}}
    has been executed before using resStratification.
  \item The 'population' parameter helps filter out stratifications with very small,
    potentially insignificant clusters.
  \item The 'stratification_similarity' parameter allows for fine-tuning the balance
    between diversity and similarity in the returned stratifications.
  \item When exploring results, it's often useful to try different combinations of
    'population' and 'stratification_similarity' values to understand the
    characteristics of your clustering solutions.
}
}
\examples{
data("BreastCancerWisconsin", package = "ClustAll")
wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
wdbc <- wdbc[1:15,1:8]
obj_noNA <- createClustAll(data = wdbc)
\donttest{
obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
resStratification(Object = obj_noNA1, population = 0.05,
                  stratification_similarity = 0.88, all = FALSE)
}

}
\seealso{
\code{\link{runClustAll}}, \code{\link{plotJACCARD}},
\code{\link{cluster2data}}, \code{\link{ClustAllObject-class}}
}
