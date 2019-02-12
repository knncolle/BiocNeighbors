\name{findNeighbors}
\alias{findNeighbors}
\docType{methods}

\alias{findNeighbors,missing,missing-method}

\alias{findNeighbors,missing,KmknnParam-method}
\alias{findNeighbors,KmknnIndex,missing-method}
\alias{findNeighbors,KmknnIndex,KmknnParam-method}

\alias{findNeighbors,missing,VptreeParam-method}
\alias{findNeighbors,VptreeIndex,missing-method}
\alias{findNeighbors,VptreeIndex,VptreeParam-method}

\title{Find all neighbors in range}
\description{Find all neighbors within a given distance for each point in a data set, using exact or approximate algorithms.}

\usage{
findNeighbors(X, threshold, subset=NULL, get.index=TRUE, get.distance=TRUE,
    BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM)
}

\arguments{
    \item{X}{A numeric data matrix where rows are points and columns are dimensions.}
    \item{threshold}{A numeric scalar or vector specifying the maximum distance for considering neighbors.}
    \item{subset}{A vector specifying the subset of points in \code{X} to search.}
    \item{get.index}{A logical scalar indicating whether to return row indices of the neighbors.}
    \item{get.distance}{A logical scalar indicating whether to return distances to neighbors.}
    \item{BPPARAM}{A \linkS4class{BiocParallelParam} class for parallelization.}
    \item{...}{Further arguments to pass to specific methods.}
    \item{BNINDEX}{A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
        This can be missing if \code{BNPARAM} is supplied, see Details.}
    \item{BNPARAM}{A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
        This can be missing if \code{BNINDEX} is supplied, see Details.}
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
}

\details{
The class of \code{BNINDEX} and \code{BNPARAM} will determine the dispatch to specific functions.
Only one of these arguments needs to be defined to resolve dispatch.
However, if both are defined, they cannot specify different algorithms.

If \code{BNINDEX} is supplied, \code{X} does not need to be specified.
In fact, any value of \code{X} will be ignored as all necessary information for the search is already present in \code{BNINDEX}.

If both \code{BNINDEX} and \code{BNPARAM} are missing, the function will default to the KMKNN algorithm by setting \code{BNPARAM=KmknnParam()}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{rangeFindKmknn}}
and \code{\link{rangeFindVptree}} for specific methods.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
k.out <- findNeighbors(Y, threshold=1)
a.out <- findNeighbors(Y, threshold=1, BNPARAM=VptreeParam())

k.dex <- buildKmknn(Y)
k.out2 <- findNeighbors(Y, threshold=1, BNINDEX=k.dex)
k.out3 <- findNeighbors(Y, threshold=1, BNINDEX=k.dex, BNPARAM=KmknnParam())

v.dex <- buildVptree(Y)
v.out2 <- findNeighbors(Y, threshold=1, BNINDEX=v.dex)
v.out3 <- findNeighbors(Y, threshold=1, BNINDEX=v.dex, BNPARAM=VptreeParam())
}

