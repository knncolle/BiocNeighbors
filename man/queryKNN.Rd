\name{queryKNN}
\alias{queryKNN}
\docType{methods}

\alias{queryKNN,missing,missing-method}
\alias{queryKNN,BiocNeighborIndex,missing-method}
\alias{queryKNN,missing,BiocNeighborParam-method}

\alias{queryKNN,NULL,missing-method}
\alias{queryKNN,missing,NULL-method}
\alias{queryKNN,NULL,NULL-method}

\alias{queryKNN,NULL,KmknnParam-method}
\alias{queryKNN,KmknnIndex,NULL-method}
\alias{queryKNN,KmknnIndex,KmknnParam-method}

\alias{queryKNN,NULL,VptreeParam-method}
\alias{queryKNN,VptreeIndex,NULL-method}
\alias{queryKNN,VptreeIndex,VptreeParam-method}

\alias{queryKNN,NULL,AnnoyParam-method}
\alias{queryKNN,AnnoyIndex,NULL-method}
\alias{queryKNN,AnnoyIndex,AnnoyParam-method}

\title{Query k-nearest neighbors }
\description{Find the k-nearest neighbors in one data set for each point in another query data set, using exact or approximate algorithms.}

\usage{
queryKNN(..., BNINDEX=NULL, BNPARAM=NULL)
}

\arguments{
    \item{...}{Further arguments to pass to specific methods, including:
        \itemize{
            \item \code{X}, a numeric data matrix where rows are points and columns are dimensions.
            \item \code{query}, a numeric query matrix where rows are points and columns are dimensions.
            \item \code{k}, an integer scalar for the number of nearest neighbors.
            \item \code{subset}, a vector specifying the subset of points in \code{X} to search.
            \item \code{get.index}, a logical scalar indicating whether to return row indices of the neighbors.
            \item \code{get.distance}, a logical scalar indicating whether to return distances to neighbors.
            \item \code{BPPARAM}, a \linkS4class{BiocParallelParam} object for parallelization.
            \item \code{transposed}, a logical scalar indicating whether \code{query} is transposed, i.e., with columns as points.
        }
        Of these, only \code{query} and \code{k} are strictly mandatory.
    }
    \item{BNINDEX}{A \linkS4class{BiocNeighborIndex} object, or \code{NULL}.}
    \item{BNPARAM}{A \linkS4class{BiocNeighborParam} object, or \code{NULL} if \code{BININDEX} is supplied.}
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
The class of \code{BNINDEX} and \code{BNPARAM} will determine whether dispatch is performed to \code{\link{queryKmknn}} or \code{\link{queryAnnoy}}.
Only one of these arguments needs to be defined to resolve dispatch.
However, if both are defined, they cannot specify different algorithms.

If \code{BNINDEX} is supplied, \code{X} does not need to be specified.
In fact, any value of \code{X} will be ignored as all necessary information for the search is already present in \code{BNINDEX}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{queryKmknn}}, 
\code{\link{queryVptree}},
and \code{\link{queryAnnoy}} for specific methods.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
Z <- matrix(rnorm(10000), ncol=20)
str(k.out <- queryKNN(Y, Z, k=10))
str(a.out <- queryKNN(Y, Z, k=10, BNPARAM=AnnoyParam()))

k.dex <- buildKmknn(Y)
str(k.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=k.dex, BNPARAM=NULL))
str(k.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))

a.dex <- buildAnnoy(Y)
str(a.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=a.dex, BNPARAM=NULL))
str(a.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
}

