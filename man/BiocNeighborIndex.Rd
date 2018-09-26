\name{BiocNeighborIndex}
\alias{BiocNeighborIndex-class}
\docType{class}

\title{The BiocNeighborIndex class}
\description{A virtual class for indexing structures of different nearest-neighbor search algorithms.}

\details{
The BiocNeighborIndex class is a virtual base class on which other index objects are built.
There are 2 concrete subclasses:
\describe{
    \item{}{\code{\link{KmknnIndex}}: exact nearest-neighbor search with the KMKNN algorithm.}
    \item{}{\code{\link{AnnoyIndex}}: approximate nearest-neighbor search with the Annoy algorithm.}
}

These objects hold parameters specifying how each algorithm should be run on an arbitrary data set.
See the associated documentation pages for more details.
}

\seealso{
\code{\link{KmknnIndex}} and \code{\link{AnnoyIndex}} for direct constructors.

\code{\link{buildNNIndex}} for construction on an actual data set. 

\code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
}

\author{
Aaron Lun
}
