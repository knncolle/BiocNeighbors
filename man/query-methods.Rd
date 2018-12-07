\name{query methods}
\alias{queryKmknn}
\alias{queryVptree}
\alias{queryAnnoy}

\title{Query nearest neighbors}
\description{Query a dataset for nearest neighbors of points in another dataset, using a variety of algorithms.}

\usage{
queryKmknn(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(),
    precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, ...)

queryVptree(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(),
    precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE)

queryAnnoy(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(),
    precomputed=NULL, transposed=FALSE, subset=NULL, ...)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{query}{A numeric matrix of query points, containing different data points in the rows but the same number and ordering of dimensions in the columns.}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{BPPARAM}{A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.}
\item{precomputed}{A \linkS4class{BiocNeighborIndex} object of the appropriate class, generated from \code{X}.
For \code{queryKmknn}, this should be a \linkS4class{KmknnIndex} from \code{\link{buildKmknn}};
for \code{queryVptree}, this should be a \linkS4class{VptreeIndex} from \code{\link{buildVptree}};
and for \code{queryAnnoy}, this should be a \linkS4class{AnnoyIndex} from \code{\link{buildAnnoy}}.}
\item{transposed}{A logical scalar indicating whether the \code{query} is transposed, 
in which case \code{query} is assumed to contain dimensions in the rows and data points in the columns.}
\item{subset}{A vector indicating the rows of \code{query} (or columns, if \code{transposed=TRUE}) for which the nearest neighbors should be identified.}
\item{raw.index}{A logial scalar indicating whether raw column indices should be returned, see \code{?"\link{BiocNeighbors-raw-index}"}.}
\item{...}{Further arguments to pass to the respective \code{build*} function for each algorithm.}
}

\details{
All of these functions identify points in \code{X} that are the \code{k} nearest neighbors of each point in \code{query}.
\code{queryAnnoy} performs an approximate search, while \code{queryKmknn} and \code{queryVptree} are exact.
This requires both \code{X} and \code{query} to have the same number of dimensions.
Moreover, the upper bound for \code{k} is set at the number of points in \code{X}.

By default, nearest neighbors are identified for all data points within \code{query}.
If \code{subset} is specified, nearest neighbors are only detected for the query points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{queryKmknn} on the full \code{query}.

If \code{transposed=TRUE}, this function assumes that \code{query} is already transposed, which saves a bit of time by avoiding an unnecessary transposition.
Turning off \code{get.index} or \code{get.distance} may also provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search by query points across multiple processes.

If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} (e.g., with \code{\link{buildKmknn}}).
The resulting BiocNeighborIndex object can be supplied as \code{precomputed} to multiple function calls, avoiding the need to repeat index construction in each call.
Note that when \code{precomputed} is supplied, the value of \code{X} is ignored.

For exact methods, see comments in \code{?"\link{BiocNeighbors-ties}"} regarding the warnings when tied distances are observed.
For Annoy, see comments in \code{\link{buildAnnoy}} about the (lack of) randomness in the search results.
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
    This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{query}.
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
Z <- matrix(rnorm(20000), ncol=20)
out <- queryKmknn(Y, query=Z, k=5)
head(out$index)
head(out$distance)

out2 <- queryVptree(Y, query=Z, k=5)
head(out2$index)
head(out2$distance)

out3 <- queryAnnoy(Y, query=Z, k=5)
head(out$index)
head(out$distance)
}
