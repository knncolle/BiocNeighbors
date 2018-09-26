\name{buildNNIndex}
\alias{buildNNIndex}

\alias{buildNNIndex,ANY,missing-method}
\alias{buildNNIndex,ANY,KmknnParam-method}
\alias{buildNNIndex,ANY,AnnoyParam-method}

\title{Build a nearest-neighbor index}
\description{Build indices for nearest-neighbor searching with different algorithms.}

\usage{
\S4method{buildNNIndex}{ANY,missing}(X, ..., BNPARAM)

\S4method{buildNNIndex}{ANY,KmknnParam}(X, ..., BNPARAM)

\S4method{buildNNIndex}{ANY,AnnoyParam}(X, ..., BNPARAM)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{...}{Further arguments to be passed to individual methods.}
\item{BNPARAM}{A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.}
}

\details{
Supplying a \linkS4class{KmknnParam} object as \code{BNPARAM} will dispatch to \code{\link{buildKmknn}}.

Supplying an \linkS4class{AnnoyParam} object as \code{BNPARAM} will dispatch to \code{\link{buildAnnoy}}.
}

\value{
A \linkS4class{BiocNeighborIndex} object containing indexing structures for each algorithm.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} and \code{\link{buildAnnoy}} for specific methods. 
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
(k.out <- buildNNIndex(Y))
(a.out <- buildNNIndex(Y, BNPARAM=AnnoyParam()))
}
