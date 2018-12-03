\name{rangeQuery methods}
\alias{rangeQueryKmknn}
\alias{rangeQueryVptree}

\title{Query neighbors in range}
\description{Find all neighboring data points within a certain distance of a query point.}

\usage{
rangeQueryKmknn(X, query, threshold, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL,
    raw.index=FALSE, ...)

rangeQueryVptree(X, query, threshold, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL,
    raw.index=FALSE)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{query}{A numeric matrix of query points, containing different data points in the rows but the same number and ordering of dimensions in the columns.}
\item{threshold}{A positive numeric scalar specifying the maximum distance at which a point is considered a neighbor.
Alternatively, a vector containing a different distance threshold for each query point.}
\item{get.index}{A logical scalar indicating whether the indices of the neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the neighbors should be recorded.}
\item{BPPARAM}{A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.}
\item{precomputed}{A \linkS4class{BiocNeighborIndex} object of the appropriate class, generated from \code{X}.
For \code{rangeFindKmknn}, this should be a \linkS4class{KmknnIndex}, while for \code{rangeFindVptree}, this should be a \linkS4class{VptreeIndex}.}
\item{transposed}{A logical scalar indicating whether the \code{query} is transposed, 
in which case \code{query} is assumed to contain dimensions in the rows and data points in the columns.}
\item{subset}{A vector indicating the rows of \code{query} (or columns, if \code{transposed=TRUE}) for which the neighbors should be identified.}
\item{raw.index}{A logial scalar indicating whether raw column indices to \code{precomputed$data} should be returned.}
\item{...}{Further arguments to pass to \code{\link{buildKmknn}} if \code{precomputed=NULL}.}
}

\details{
This function identifies points in \code{X} that are neighbors (i.e., within a distance \code{threshold}) of each point in \code{query}.
The exact implementation can either use the KMKNNN approach or a VP tree.
This requires both \code{X} and \code{query} to have the same number of variables.

By default, neighbors are identified for all data points within \code{query}.
If \code{subset} is specified, neighbors are only detected for the query points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{queryNeighbors} on the full \code{query} (i.e., with \code{subset=NULL}).

If \code{threshold} is a vector, each entry is assumed to specify a (possibly different) threshold for each point in \code{query}.
If \code{subset} is also specified, each entry is assumed to specify a threshold for each point in \code{subset}.
An error will be raised if \code{threshold} is a vector of incorrect length.

If \code{transposed=TRUE}, this function assumes that \code{query} is already transposed, which saves a bit of time by avoiding an unnecessary transposition.
Turning off \code{get.index} or \code{get.distance} may also provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search by query points across multiple processes.

If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} (e.g., with \code{\link{buildKmknn}}) 
and pass the resulting BiocNeighborIndex object to \code{precomputed}.
Advanced users can also set \code{raw.index=TRUE}, which returns indices of neighbors in the reordered data set in \code{precomputed}.
This may be useful when dealing with multiple queries to a common precomputed object.
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
        This is a list of integer vectors where each entry corresponds to a point (denoted here as \eqn{i}) in \code{query}.
        The vector for \eqn{i} contains the set of row indices of all points in \code{X} that lie within \code{threshold} of point \eqn{i}.
        Points in each vector are not ordered, and \eqn{i} will always be included in its own set.
    \item \code{distance}, if \code{get.distance=TRUE}.
        This is a list of numeric vectors where each entry corresponds to a point (as above) and contains the distances of the neighbors from \eqn{i}.
        Elements of each vector in \code{distance} match to elements of the corresponding vector in \code{index}.
}

If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.

If \code{raw.index=TRUE}, the values in \code{index} refer to \emph{columns} of \code{\link{KmknnIndex_clustered_data}(precomputed)}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} or \code{\link{buildVptree}} to build an index ahead of time.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
Z <- matrix(rnorm(20000), ncol=20)
out <- rangeQueryKmknn(Y, query=Z, threshold=1)
head(out$index)
head(out$distance)

out2 <- rangeQueryVptree(Y, query=Z, threshold=1)
head(out2$index)
head(out2$distance)

}
