\name{queryNeighbors}
\alias{queryNeighbors}
\docType{methods}

\alias{queryNeighbors,missing,missing-method}

\alias{queryNeighbors,missing,KmknnParam-method}
\alias{queryNeighbors,KmknnIndex,missing-method}
\alias{queryNeighbors,KmknnIndex,KmknnParam-method}

\alias{queryNeighbors,missing,VptreeParam-method}
\alias{queryNeighbors,VptreeIndex,missing-method}
\alias{queryNeighbors,VptreeIndex,VptreeParam-method}

\title{Query all neighbors}
\description{Find all neighbors in one data set that are in range of each point in another query data set, using exact or approximate algorithms.}

\usage{
queryNeighbors(X, query, threshold, subset=NULL, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), transposed=FALSE, ..., BNINDEX, BNPARAM) 
}

\arguments{
    \item{X}{A numeric data matrix where rows are points and columns are dimensions.}
    \item{query}{A numeric query matrix where rows are points and columns are dimensions.}
    \item{threshold}{A numeric scalar or vector specifying the maximum distance for considering neighbors.}
    \item{subset}{A vector specifying the subset of points in \code{X} to search.}
    \item{get.index}{A logical scalar indicating whether to return row indices of the neighbors.}
    \item{get.distance}{A logical scalar indicating whether to return distances to neighbors.}
    \item{BPPARAM}{A \linkS4class{BiocParallelParam} object for parallelization.}
    \item{transposed}{A logical scalar indicating whether \code{query} is transposed, i.e., with columns as points.}
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
    This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{query}.
    The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
    \item \code{distance}, if \code{get.distance=TRUE}.
    This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
}

If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.
}

\details{
The class of \code{BNINDEX} and \code{BNPARAM} will determine dispatch to specific methods.
Only one of these arguments needs to be defined to resolve dispatch.
However, if both are defined, they cannot specify different algorithms.

If \code{BNINDEX} is supplied, \code{X} does not need to be specified.
In fact, any value of \code{X} will be ignored as all necessary information for the search is already present in \code{BNINDEX}.
Similarly, any parameters in \code{BNPARAM} will be ignored.

If both \code{BNINDEX} and \code{BNPARAM} are missing, the function will default to the KMKNN algorithm by setting \code{BNPARAM=KmknnParam()}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{rangeQueryKmknn}} and  
\code{\link{rangeQueryVptree}} for specific methods.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
Z <- matrix(rnorm(10000), ncol=20)
k.out <- queryNeighbors(Y, Z, threshold=1)
v.out <- queryNeighbors(Y, Z, threshold=1, BNPARAM=VptreeParam())

k.dex <- buildKmknn(Y)
k.out2 <- queryNeighbors(Y,Z,  threshold=1, BNINDEX=k.dex)
k.out3 <- queryNeighbors(Y,Z,  threshold=1, BNINDEX=k.dex, BNPARAM=KmknnParam())

v.dex <- buildVptree(Y)
v.out2 <- queryNeighbors(Y,Z,  threshold=1, BNINDEX=v.dex)
v.out3 <- queryNeighbors(Y,Z,  threshold=1, BNINDEX=v.dex, BNPARAM=VptreeParam())
}

