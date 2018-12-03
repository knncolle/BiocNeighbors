\name{find methods}
\alias{findKmknn}
\alias{findVptree}
\alias{findAnnoy}

\title{Find nearest neighbors}
\description{Use the KMKNN (K-means for k-nearest neighbors) algorithm to identify nearest neighbors from a dataset.}

\usage{
findKmknn(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, raw.index=FALSE, ...)

findVptree(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, raw.index=FALSE)

findAnnoy(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, ...)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{BPPARAM}{A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.}
\item{precomputed}{A \linkS4class{BiocNeighborIndex} object of the appropriate class, generated from \code{X}.
For \code{findKmknn}, this should be a \linkS4class{KmknnIndex};
for \code{findVptree}, this should be a \linkS4class{VptreeIndex};
and for \code{findAnnoy}, this should be a \linkS4class{AnnoyIndex}.}
\item{subset}{A vector indicating the rows of \code{X} for which the nearest neighbors should be identified.}
\item{raw.index}{A logial scalar indicating whether raw column indices should be returned, see \code{?"\link{BiocNeighbors-raw-index}"}.}
\item{...}{Further arguments to pass to \code{\link{buildKmknn}} (for \code{findKmknn}) or \code{\link{buildAnnoy}} (for \code{findAnnoy}) if \code{precomputed=NULL}.}
}

\details{
All of these functions identify points in \code{X} that are the \code{k} nearest neighbors of each other point.
\code{findAnnoy} performs an approximate search, while \code{findKmknn} and \code{findVptree} are exact.
The upper bound for \code{k} is set at the number of points in \code{X} minus 1.

By default, nearest neighbors are identified for all data points within \code{X}.
If \code{subset} is specified, nearest neighbors are only detected for the points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findKmknn} with \code{subset=NULL}.

Turning off \code{get.index} or \code{get.distance} will not return the corresponding matrices in the output.
This may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search across multiple workers, which should increase speed proportionally (in theory) to the number of cores.

If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} (e.g., with \code{\link{buildKmknn}}).
The resulting BiocNeighborIndex object can be supplied as \code{precomputed} to multiple function calls, avoiding the need to repeat index construction in each call.
Note that when \code{precomputed} is supplied, the value of \code{X} is completely ignored.

For exact methods, see comments in \code{?"\link{BiocNeighbors-ties}"} regarding the warnings when tied distances are observed.
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
    This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{X}.
    The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
    \item \code{distance}, if \code{get.distance=TRUE}.
    This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
}

If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.

See \code{?"\link{BiocNeighbors-raw-index}"} for an explanation of the output when \code{raw.index=TRUE} for the functions that support it.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}}, 
\code{\link{buildVptree}},
or \code{\link{buildAnnoy}} to build an index ahead of time.

See \code{?"\link{BiocNeighbors-algorithms}"} for an overview of the available algorithms.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- findKmknn(Y, k=8)
head(out$index)
head(out$distance)

out2 <- findVptree(Y, k=8)
head(out2$index)
head(out2$distance)

out3 <- findAnnoy(Y, k=8)
head(out3$index)
head(out3$distance)
}
