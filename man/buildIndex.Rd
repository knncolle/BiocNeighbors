\name{buildIndex}
\alias{buildIndex}
\docType{methods}

\alias{buildIndex,missing-method}
\alias{buildIndex,NULL-method}
\alias{buildIndex,KmknnParam-method}
\alias{buildIndex,VptreeParam-method}
\alias{buildIndex,AnnoyParam-method}

\title{Build a nearest-neighbor index}
\description{Build indices for nearest-neighbor searching with different algorithms.}

\usage{
buildIndex(..., BNPARAM=NULL)
}

\arguments{
\item{...}{Further arguments to be passed to individual methods.
The mandatory argument for all methods is \code{X}, a numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{BNPARAM}{A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
This defaults to a \linkS4class{KmknnParam} object if \code{NULL}.}
}

\details{
Supplying a \linkS4class{KmknnParam} object as \code{BNPARAM} will dispatch to \code{\link{buildKmknn}}.

Supplying a \linkS4class{VptreeParam} object as \code{BNPARAM} will dispatch to \code{\link{buildVptree}}.

Supplying an \linkS4class{AnnoyParam} object as \code{BNPARAM} will dispatch to \code{\link{buildAnnoy}}.
}

\value{
A \linkS4class{BiocNeighborIndex} object containing indexing structures for each algorithm.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}},
\code{\link{buildVptree}},
and \code{\link{buildAnnoy}} for specific methods. 
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
(k.out <- buildIndex(Y))
(a.out <- buildIndex(Y, BNPARAM=AnnoyParam()))
}
