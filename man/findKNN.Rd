\name{findKNN}
\alias{findKNN}

\alias{findKNN,ANY,missing,missing-method}
\alias{findKNN,ANY,BiocNeighborIndex,missing-method}
\alias{findKNN,ANY,missing,BiocNeighborParam-method}

\alias{findKNN,ANY,NULL,KmknnParam-method}
\alias{findKNN,ANY,KmknnIndex,NULL-method}
\alias{findKNN,ANY,KmknnIndex,KmknnParam-method}

\alias{findKNN,ANY,NULL,AnnoyParam-method}
\alias{findKNN,ANY,AnnoyIndex,NULL-method}
\alias{findKNN,ANY,AnnoyIndex,AnnoyParam-method}

\title{Find k-nearest neighbors}
\description{Find the k-nearest neighbors for each point in a data set, using exact or approximate algorithms.}

\usage{
findKNN(X, ..., BNINDEX=NULL, BNPARAM=KmknnParam())
}

\arguments{
    \item{X}{A numeric data matrix where rows are points and columns are dimensions.}
    \item{...}{Further arguments to pass to specific methods, including:
        \itemize{
            \item \code{k}, an integer scalar for the number of nearest neighbors.
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

