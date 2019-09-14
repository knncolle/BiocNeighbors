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
\description{Find all neighbors in one data set that are in range of each point in another query data set.}

\usage{
queryNeighbors(X, query, threshold, ..., BNINDEX, BNPARAM) 
}

\arguments{
    \item{X}{A numeric data matrix where rows are points and columns are dimensions.}
    \item{query}{A numeric query matrix where rows are points and columns are dimensions.}
    \item{threshold}{A numeric scalar or vector specifying the maximum distance for considering neighbors.}
    \item{...}{Further arguments to pass to specific methods.
    This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance} \code{BPPARAM} and \code{raw.index}.
    See \code{?"\link{rangeQuery-methods}"} for more details.}
    \item{BNINDEX}{A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
        This can be missing if \code{BNPARAM} is supplied, see Details.}
    \item{BNPARAM}{A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
        This can be missing if \code{BNINDEX} is supplied, see Details.}
}

\value{
A list is returned containing \code{index}, a list of integer vectors specifying the identities of the neighbors of each point;
and \code{distance}, a list of numeric vectors containing the distances to those neighbors.
See \code{?"\link{rangeQuery-methods}"} for more details.
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

