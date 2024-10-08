% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustAll_ClustAllObject_Class.R
\docType{class}
\name{logicalOrNA-class}
\alias{logicalOrNA-class}
\title{Class Union: logicalOrNA}
\description{
This class union allows for flexibility in method signatures and slot
definitions by accepting either a logical value, NULL, or a missing value.
It is particularly useful when a slot or function parameter might contain
a boolean flag but could also be empty, unspecified, or explicitly set to NULL.
}
\details{
The logicalOrNA class union includes:
\itemize{
  \item logical: A standard R logical value (TRUE or FALSE)
  \item NULL: Representing an empty or unset value
  \item missing: Allowing for unspecified parameters in function calls
}

This union is useful in scenarios where:
\itemize{
  \item A function might return a logical result or NULL if no result is available
  \item A slot in an S4 object could contain a logical flag or be empty
  \item A function parameter could accept a logical input, but also work with default settings if nothing is provided
}
}
\seealso{
\code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
}
