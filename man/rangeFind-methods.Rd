\name{rangeFind methods}
\alias{rangeFindKmknn}
\alias{rangeFindVptree}

\title{Find all neighbors in range}
\description{Find all neighboring data points within a certain distance of each point.}

\usage{
rangeFindKmknn(X, threshold, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, 
    raw.index=FALSE, ...)

rangeFindVptree(X, threshold, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, 
    raw.index=FALSE)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{threshold}{A positive numeric scalar specifying the maximum distance at which a point is considered a neighbor.
Alternatively, a vector containing a different distance threshold for each point.}
\item{get.index}{A logical scalar indicating whether the indices of the neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the neighbors should be recorded.}
\item{BPPARAM}{A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.}
\item{precomputed}{A \linkS4class{BiocNeighborIndex} object of the appropriate class, generated from \code{X}.
For \code{rangeFindKmknn}, this should be a \linkS4class{KmknnIndex}, while for \code{rangeFindVptree}, this should be a \linkS4class{VptreeIndex}.}
\item{subset}{A vector indicating the rows of \code{X} for which the neighbors should be identified.}
\item{raw.index}{A logial scalar indicating whether raw column indices should be returned, see \code{?"\link{BiocNeighbors-raw-index}"}.}
\item{...}{Further arguments to pass to \code{\link{buildKmknn}} if \code{precomputed=NULL}.}
}

\details{
This function identifies all points in \code{X} that within \code{threshold} of each point in \code{X}.
For Euclidean distances, this is equivalent to identifying all points in a hypersphere centered around the point of interest.
The exact implementation can either use the KMKNNN approach or a VP tree.

By default, a search is performed for each data point in \code{X}, but it can be limited to a specified subset of points with \code{subset}.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findNeighbors} with \code{subset=NULL}.

If \code{threshold} is a vector, each entry is assumed to specify a (possibly different) threshold for each point in \code{X}.
If \code{subset} is also specified, each entry is assumed to specify a threshold for each point in \code{subset}.
An error will be raised if \code{threshold} is a vector of incorrect length.

Turning off \code{get.index} or \code{get.distance} may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search by query points, which usually provides a linear increase in speed.

If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} (e.g., with \code{\link{buildKmknn}}).
The resulting BiocNeighborIndex object can be supplied as \code{precomputed} to multiple function calls, avoiding the need to repeat index construction in each call.
Note that when \code{precomputed} is supplied, the value of \code{X} is ignored.
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
        This is a list of integer vectors where each entry corresponds to a point (denoted here as \eqn{i}) in \code{X}.
        The vector for \eqn{i} contains the set of row indices of all points in \code{X} that lie within \code{threshold} of point \eqn{i}.
        Points in each vector are not ordered, and \eqn{i} will always be included in its own set.
    \item \code{distance}, if \code{get.distance=TRUE}.
        This is a list of numeric vectors where each entry corresponds to a point (as above) and contains the distances of the neighbors from \eqn{i}.
        Elements of each vector in \code{distance} match to elements of the corresponding vector in \code{index}.
}

If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.

See \code{?"\link{BiocNeighbors-raw-index}"} for an explanation of the output when \code{raw.index=TRUE}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} or \code{\link{buildVptree}} to build an index ahead of time.

See \code{?"\link{BiocNeighbors-algorithms}"} for an overview of the available algorithms.
}

\examples{
Y <- matrix(runif(100000), ncol=20)
out <- rangeFindKmknn(Y, threshold=1)
out2 <- rangeFindVptree(Y, threshold=1)
}

