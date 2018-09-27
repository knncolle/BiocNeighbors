\name{BiocNeighborIndex}
\alias{BiocNeighborIndex-class}
\docType{class}

\alias{show,BiocNeighborIndex-method}
\alias{dimnames,BiocNeighborIndex-method}

\title{The BiocNeighborIndex class}
\description{A virtual class for indexing structures of different nearest-neighbor search algorithms.}

\details{
The BiocNeighborIndex class is a virtual base class on which other index objects are built.
There are 2 concrete subclasses:
\describe{
    \item{}{\code{\link{KmknnIndex}}: exact nearest-neighbor search with the KMKNN algorithm.}
    \item{}{\code{\link{AnnoyIndex}}: approximate nearest-neighbor search with the Annoy algorithm.}
}

These objects hold indexing structures for a given data set - see the associated documentation pages for more details.
It also retains information about the dimensionality of the input data as well as the sample names.
}

\section{Methods}{
\describe{
    \item{\code{show(object)}:}{Display the class and dimensions of a BiocNeighborIndex \code{object}.}
    \item{\code{dimnames(x)}:}{Display the dimension names of a BiocNeighborIndex \code{x}.}
}
}

\seealso{
\code{\link{KmknnIndex}} and \code{\link{AnnoyIndex}} for direct constructors.

\code{\link{buildNNIndex}} for construction on an actual data set. 

\code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
}

\author{
Aaron Lun
}
