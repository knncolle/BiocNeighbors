\name{BiocNeighborParam}
\alias{BiocNeighborParam-class}
\docType{class}

\alias{show,BiocNeighborParam-method}
\alias{bndistance,BiocNeighborParam-method}

\title{The BiocNeighborParam class}
\description{A virtual class for specifying the type of nearest-neighbor search algorithm and associated parameters.}

\details{
The BiocNeighborParam class is a virtual base class on which other parameter objects are built.
There are currently 4 concrete subclasses:
\describe{
    \item{}{\code{\link{KmknnParam}}: exact nearest-neighbor search with the KMKNN algorithm.}
    \item{}{\code{\link{VptreeParam}}: exact nearest-neighbor search with the VP tree algorithm.}
    \item{}{\code{\link{AnnoyParam}}: approximate nearest-neighbor search with the Annoy algorithm.}
    \item{}{\code{\link{HnswParam}}: approximate nearest-neighbor search with the HNSW algorithm.}
}

These objects hold parameters specifying how each algorithm should be run on an arbitrary data set.
See the associated documentation pages for more details.
}

\section{Methods}{
\describe{
\item{\code{show(object)}:}{Display the class of a BiocNeighborParam \code{object}.}
\item{\code{bndistance(object)}:}{Return a string specifying the distance metric to be used for searching, usually \code{"Euclidean"} or \code{"Manhattan"}.}
}
}

\seealso{
\code{\link{KmknnParam}},
\code{\link{VptreeParam}},
\code{\link{AnnoyParam}},
and \code{\link{HnswParam}} for constructors.

\code{\link{buildIndex}}, \code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
}

\author{
Aaron Lun
}
