\name{BiocNeighborIndex}
\alias{BiocNeighborIndex-class}
\docType{class}

\alias{show,BiocNeighborIndex-method}
\alias{dim,BiocNeighborIndex-method}
\alias{dimnames,BiocNeighborIndex-method}

\alias{bndata}
\alias{bndata,BiocNeighborIndex-method}

\alias{bndistance}
\alias{bndistance,BiocNeighborIndex-method}

\alias{bnorder}
\alias{bnorder,KmknnIndex-method}
\alias{bnorder,VptreeIndex-method}
\alias{bnorder,AnnoyIndex-method}
\alias{bnorder,HnswIndex-method}

\title{The BiocNeighborIndex class}
\description{A virtual class for indexing structures of different nearest-neighbor search algorithms.}

\details{
The BiocNeighborIndex class is a virtual base class on which other index objects are built.
There are 4 concrete subclasses:
\describe{
    \item{}{\code{\link{KmknnIndex}}: exact nearest-neighbor search with the KMKNN algorithm.}
    \item{}{\code{\link{VptreeIndex}}: exact nearest-neighbor search with a VP tree.}
    \item{}{\code{\link{AnnoyIndex}}: approximate nearest-neighbor search with the Annoy algorithm.}
    \item{}{\code{\link{HnswIndex}}: approximate nearest-neighbor search with the HNSW algorithm.}
}

These objects hold indexing structures for a given data set - see the associated documentation pages for more details.
It also retains information about the input data as well as the sample names.
}

\section{Methods}{
The main user-accessible methods are:
\describe{
    \item{\code{show(object)}:}{Display the class and dimensions of a BiocNeighborIndex \code{object}.}
    \item{\code{dim(x)}:}{Return the dimensions of a BiocNeighborIndex \code{x}, in terms of the matrix used to construct it.}
    \item{\code{dimnames(x)}:}{Return the dimension names of a BiocNeighborIndex \code{x}.
        Only the row names of the input matrix are stored, in the same order.
    }
}

More advanced methods (intended for developers of other packages) are:
\describe{
    \item{\code{bndata(object)}:}{Return a numeric matrix containing the data used to construct \code{object}.
        Each column should represent a data point and each row should represent a variable 
        (i.e., it is transposed compared to the usual input, for efficient column-major access in C++ code).
        Columns may be reordered from the input matrix according to \code{bnorder(object)}.
    }
    \item{\code{bnorder(object)}:}{Return an integer vector specifying the new ordering of columns in \code{bndata(object)}.
        This generally only needs to be considered if \code{raw.index=TRUE}, see \code{?"\link{BiocNeighbors-raw-index}"}.
    }
    \item{\code{bndistance(object)}:}{Return a string specifying the distance metric to be used for searching, usually \code{"Euclidean"} or \code{"Manhattan"}.
        Obviously, this should be the same as the distance metric used for constructing the index.
    }
}
}

\seealso{
\code{\link{KmknnIndex}},
\code{\link{VptreeIndex}},
\code{\link{AnnoyIndex}},
and \code{\link{HnswIndex}} for direct constructors.

\code{\link{buildIndex}} for construction on an actual data set. 

\code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
}

\author{
Aaron Lun
}
