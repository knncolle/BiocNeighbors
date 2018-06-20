\name{findNeighbors}
\alias{findNeighbors}

\title{Find all neighbors}
\description{Find all neighboring data points within a certain distance.}

\usage{
findNeighbors(X, threshold, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{threshold}{A positive numeric scalar specifying the maximum distance at which a point is considered a neighbor.}
\item{get.index}{A logical scalar indicating whether the indices of the neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the neighbors should be recorded.}
\item{BPPARAM}{A BiocParallelParam object indicating how the search should be parallelized.}
\item{precomputed}{The precomputed output of \code{\link{precluster}} on \code{X}.}
\item{subset}{A vector indicating the rows of \code{X} for which the neighbors should be identified.}
}

\details{
This function uses the same algorithm described in \code{\link{findKNN}} to identify all points in \code{X} that within \code{threshold} of each point in \code{X}.
For Euclidean distances, this is equivalent to identifying all points in a hypersphere centered around the point of interest.

By default, neighbors are identified for all data points within \code{X}, but the search can be limited to a subset with \code{subset}.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findNeighbors} with \code{subset=NULL}.

Turning off \code{get.index} or \code{get.distance} may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search by query points, which usually provides a linear increase in speed.

If multiple queries are to be performed to the same \code{X}, it may be beneficial to use \code{\link{precluster}} directly to precompute the clustering.
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
        This is a list of integer vectors where each entry corresponds to a point (denoted here as \eqn{i}) in \code{X} or a subset thereof if \code{subset} is not \code{NULL}.
        The vector for \eqn{i} contains the set of row indices of all points in \code{X} that lie within \code{threshold} of point \eqn{i}.
        Points in each vector are not ordered, and \eqn{i} will always be included in its own set.
    \item \code{distance}, if \code{get.distance=TRUE}.
        This is a list of numeric vectors where each entry corresponds to a point (as above) and contains the distances of the neighbors from \eqn{i}.
        Elements of each vector in \code{distance} match to elements of the corresponding vector in \code{index}.
}
}

\author{
Aaron Lun
}

\seealso{
\code{\link{precluster}},
\code{\link{findKNN}}
}

\examples{
Y <- matrix(runif(100000), ncol=20)
out <- findNeighbors(Y, threshold=1)
}

