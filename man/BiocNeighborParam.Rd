\name{BiocNeighborParam}
\alias{BiocNeighborParam-class}
\docType{class}

\alias{show,BiocNeighborParam-method}

\title{The BiocNeighborParam class}
\description{A virtual class for specifying the type of nearest-neighbor search algorithm and associated parameters.}

\details{
The BiocNeighborParam class is a virtual base class on which other parameter objects are built.
There are 2 concrete subclasses:
\describe{
    \item{}{\code{\link{KmknnParam}}: exact nearest-neighbor search with the KMKNN algorithm.}
    \item{}{\code{\link{AnnoyParam}}: approximate nearest-neighbor search with the Annoy algorithm.}
}

These objects hold parameters specifying how each algorithm should be run on an arbitrary data set.
See the associated documentation pages for more details.
}

\section{Methods}{
\describe{
\item{\code{show(object)}:}{Display the class of a BiocNeighborParam \code{object}.}
}
}

\seealso{
\code{\link{KmknnParam}} and \code{\link{AnnoyParam}} for constructors.

\code{\link{buildNNIndex}}, \code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
}

\author{
Aaron Lun
}
