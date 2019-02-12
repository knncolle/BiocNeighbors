\name{queryKNN}
\alias{queryKNN}
\docType{methods}

\alias{queryKNN,missing,missing-method}

\alias{queryKNN,missing,KmknnParam-method}
\alias{queryKNN,KmknnIndex,missing-method}
\alias{queryKNN,KmknnIndex,KmknnParam-method}

\alias{queryKNN,missing,VptreeParam-method}
\alias{queryKNN,VptreeIndex,missing-method}
\alias{queryKNN,VptreeIndex,VptreeParam-method}

\alias{queryKNN,missing,AnnoyParam-method}
\alias{queryKNN,AnnoyIndex,missing-method}
\alias{queryKNN,AnnoyIndex,AnnoyParam-method}

\alias{queryKNN,missing,HnswParam-method}
\alias{queryKNN,HnswIndex,missing-method}
\alias{queryKNN,HnswIndex,HnswParam-method}

\title{Query k-nearest neighbors }
\description{Find the k-nearest neighbors in one data set for each point in another query data set, using exact or approximate algorithms.}

\usage{
queryKNN(X, query, k, subset=NULL, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), transposed=FALSE, ..., BNINDEX, BNPARAM) 
}

\arguments{
    \item{X}{A numeric data matrix where rows are points and columns are dimensions.}
    \item{query}{A numeric query matrix where rows are points and columns are dimensions.}
    \item{k}{An integer scalar for the number of nearest neighbors.}
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

If both \code{BNINDEX} and \code{BNPARAM} are missing, the function will default to the KMKNN algorithm by setting \code{BNPARAM=KmknnParam()}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{queryKmknn}}, 
\code{\link{queryVptree}},
\code{\link{queryAnnoy}} 
and \code{\link{queryHnsw}} for specific methods.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
Z <- matrix(rnorm(10000), ncol=20)
str(k.out <- queryKNN(Y, Z, k=10))
str(a.out <- queryKNN(Y, Z, k=10, BNPARAM=AnnoyParam()))

k.dex <- buildKmknn(Y)
str(k.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=k.dex))
str(k.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))

a.dex <- buildAnnoy(Y)
str(a.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=a.dex))
str(a.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
}

