% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/findDistance.R
\name{findDistance}
\alias{findDistance}
\alias{findDistance,matrix,ANY-method}
\alias{findDistance,BiocNeighborGenericIndex,ANY-method}
\alias{findDistance,matrix-method}
\alias{findDistance,BiocNeighborGenericIndex-method}
\title{Distance to the k-th nearest neighbor}
\usage{
findDistance(X, k, num.threads = 1, subset = NULL, ..., BNPARAM = NULL)
}
\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
Alternatively, a prebuilt \linkS4class{BiocNeighborIndex} object from \code{\link{buildIndex}}.}

\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.

Alternatively, an integer vector of length equal to the number of points in \code{X}, specifying the number of neighbors to identify for each point.
If \code{subset} is provided, this should have length equal to the length of \code{subset}.
Users should wrap this vector in an \link{AsIs} class to distinguish length-1 vectors from integer scalars.

All \code{k} should be less than or equal to the number of points in \code{X} minus 1, otherwise the former will be capped at the latter with a warning.}

\item{num.threads}{Integer scalar specifying the number of threads to use for the search.}

\item{subset}{An integer, logical or character vector specifying the indices of points in \code{X} for which the nearest neighbors should be identified.
This yields the same result as (but is more efficient than) subsetting the output matrices after computing neighbors for all points.}

\item{...}{Further arguments to pass to \code{\link{buildIndex}} when \code{X} is not an external pointer.}

\item{BNPARAM}{A \linkS4class{BiocNeighborParam} object specifying how the index should be constructed.
If \code{NULL}, this defaults to a \linkS4class{KmknnParam}.
Ignored if \code{x} contains a prebuilt index.}
}
\value{
Numeric vector of length equal to the number of points in \code{X} (or \code{subset}, if provided),
containing the distance from each point to its \code{k}-th nearest neighbor.
This is equivalent to but more memory efficient than using \code{\link{findKNN}} and subsetting to the last distance.
}
\description{
Find the distance to the k-th nearest neighbor for each point in a dataset.
}
\details{
If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
The resulting pointer object can be supplied as \code{X} to multiple \code{findDistance} calls, avoiding the need to repeat index construction in each call.
}
\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- findDistance(Y, k=8)
summary(out)

}
\seealso{
\code{\link{buildIndex}}, to build an index ahead of time.
}
\author{
Aaron Lun
}
