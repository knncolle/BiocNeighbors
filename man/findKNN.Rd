\name{findKNN}
\alias{findKNN}
\docType{methods}

\alias{findKNN,missing,missing-method}
\alias{findKNN,BiocNeighborIndex,missing-method}
\alias{findKNN,missing,BiocNeighborParam-method}

\alias{findKNN,NULL,NULL-method}

\alias{findKNN,NULL,KmknnParam-method}
\alias{findKNN,KmknnIndex,NULL-method}
\alias{findKNN,KmknnIndex,KmknnParam-method}

\alias{findKNN,NULL,AnnoyParam-method}
\alias{findKNN,AnnoyIndex,NULL-method}
\alias{findKNN,AnnoyIndex,AnnoyParam-method}

\title{Find k-nearest neighbors}
\description{Find the k-nearest neighbors for each point in a data set, using exact or approximate algorithms.}

\usage{
findKNN(..., BNINDEX=NULL, BNPARAM=NULL)
}

\arguments{
    \item{...}{Further arguments to pass to specific methods, including:
        \itemize{
            \item \code{X}, a numeric data matrix where rows are points and columns are dimensions.
            \item \code{k}, an integer scalar for the number of nearest neighbors.
            This is the only strictly mandatory parameter.
            \item \code{subset}, a vector specifying the subset of points in \code{X} to search.
            \item \code{get.index}, a logical scalar indicating whether to return row indices of the neighbors.
            \item \code{get.distance}, a logical scalar indicating whether to return distances to neighbors.
            \item \code{BPPARAM}, a \linkS4class{BiocParallelParam} class for parallelization.
        }
    }
    \item{BNINDEX}{A \linkS4class{BiocNeighborIndex} object, or \code{NULL}.}
    \item{BNPARAM}{A \linkS4class{BiocNeighborParam} object, or \code{NULL} if \code{BININDEX} is supplied.}
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
The class of \code{BNINDEX} and \code{BNPARAM} will determine whether dispatch is performed to \code{\link{findKmknn}} or \code{\link{findAnnoy}}.
Only one of these arguments needs to be defined to resolve dispatch.
However, if both are defined, they cannot specify different algorithms.

If \code{BNINDEX} is supplied, \code{X} does not need to be specified.
In fact, any value of \code{X} will be ignored as all necessary information for the search is already present in \code{BNINDEX}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{findKmknn}} and \code{\link{findAnnoy}} for specific methods.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
str(k.out <- findKNN(Y, k=10))
str(a.out <- findKNN(Y, k=10, BNPARAM=AnnoyParam()))

k.dex <- buildKmknn(Y)
str(k.out2 <- findKNN(Y, k=10, BNINDEX=k.dex, BNPARAM=NULL))
str(k.out3 <- findKNN(Y, k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))

a.dex <- buildAnnoy(Y)
str(a.out2 <- findKNN(Y, k=10, BNINDEX=a.dex, BNPARAM=NULL))
str(a.out3 <- findKNN(Y, k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
}

