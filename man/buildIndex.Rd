\name{buildIndex}
\alias{buildIndex}
\docType{methods}

\alias{buildIndex,missing-method}
\alias{buildIndex,KmknnParam-method}
\alias{buildIndex,VptreeParam-method}
\alias{buildIndex,AnnoyParam-method}
\alias{buildIndex,HnswParam-method}

\title{Build a nearest-neighbor index}
\description{Build indices for nearest-neighbor searching with different algorithms.}

\usage{
buildIndex(X, transposed=FALSE, ..., BNPARAM)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{transposed}{Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.}
\item{...}{Further arguments to be passed to individual methods.}
\item{BNPARAM}{A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
This defaults to a \linkS4class{KmknnParam} object if no argument is supplied.}
}

\details{
Supplying a \linkS4class{KmknnParam} object as \code{BNPARAM} will dispatch to \code{\link{buildKmknn}}.

Supplying a \linkS4class{VptreeParam} object as \code{BNPARAM} will dispatch to \code{\link{buildVptree}}.

Supplying an \linkS4class{AnnoyParam} object as \code{BNPARAM} will dispatch to \code{\link{buildAnnoy}}.

Supplying an \linkS4class{HnswParam} object as \code{BNPARAM} will dispatch to \code{\link{buildHnsw}}.
}

\value{
A \linkS4class{BiocNeighborIndex} object containing indexing structures for the specified algorithm.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}},
\code{\link{buildVptree}},
\code{\link{buildAnnoy}} 
and \code{\link{buildHnsw}} for specific methods. 
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
(k.out <- buildIndex(Y))
(a.out <- buildIndex(Y, BNPARAM=AnnoyParam()))
}
