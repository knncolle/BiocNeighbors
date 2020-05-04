#' The BiocNeighborIndex class
#'
#' A virtual class for indexing structures of different nearest-neighbor search algorithms.
#'
#' @details
#' The BiocNeighborIndex class is a virtual base class on which other index objects are built.
#' There are 5 concrete subclasses:
#' \describe{
#'     \item{}{\code{\link{ExhaustiveIndex}}: exact nearest-neighbor search with the exhaustive (i.e. brute-force) algorithm.}
#'     \item{}{\code{\link{KmknnIndex}}: exact nearest-neighbor search with the KMKNN algorithm.}
#'     \item{}{\code{\link{VptreeIndex}}: exact nearest-neighbor search with a VP tree.}
#'     \item{}{\code{\link{AnnoyIndex}}: approximate nearest-neighbor search with the Annoy algorithm.}
#'     \item{}{\code{\link{HnswIndex}}: approximate nearest-neighbor search with the HNSW algorithm.} 
#' These objects hold indexing structures for a given data set - see the associated documentation pages for more details.
#' It also retains information about the input data as well as the sample names.
#' }
#' 
#' @section Methods:
#' The main user-accessible methods are:
#' \describe{
#'     \item{\code{show(object)}:}{Display the class and dimensions of a BiocNeighborIndex \code{object}.}
#'     \item{\code{dim(x)}:}{Return the dimensions of a BiocNeighborIndex \code{x}, in terms of the matrix used to construct it.}
#'     \item{\code{dimnames(x)}:}{Return the dimension names of a BiocNeighborIndex \code{x}.
#'         Only the row names of the input matrix are stored, in the same order.
#'     }
#' }
#' 
#' More advanced methods (intended for developers of other packages) are:
#' \describe{
#'     \item{\code{bndata(object)}:}{Return a numeric matrix containing the data used to construct \code{object}.
#'         Each column should represent a data point and each row should represent a variable 
#'         (i.e., it is transposed compared to the usual input, for efficient column-major access in C++ code).
#'         Columns may be reordered from the input matrix according to \code{bnorder(object)}.
#'     }
#'     \item{\code{bnorder(object)}:}{Return an integer vector specifying the new ordering of columns in \code{bndata(object)}.
#'         This generally only needs to be considered if \code{raw.index=TRUE}, see \code{?"\link{BiocNeighbors-raw-index}"}.
#'     }
#'     \item{\code{bndistance(object)}:}{Return a string specifying the distance metric to be used for searching, usually \code{"Euclidean"} or \code{"Manhattan"}.
#'         This should be the same as the distance metric used for constructing the index.
#'     }
#' }
#'
#' @seealso
#' \code{\link{KmknnIndex}},
#' \code{\link{VptreeIndex}},
#' \code{\link{AnnoyIndex}},
#' and \code{\link{HnswIndex}} for direct constructors.
#' 
#' \code{\link{buildIndex}} for construction on an actual data set.
#' 
#' \code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
#' @author Aaron Lun
#' @docType class
#' @name BiocNeighborIndex
#' @aliases BiocNeighborIndex, BiocNeighborIndex-class, show,BiocNeighborIndex, 
#'          bndistance,BiocNeighborIndex, bndata,BiocNeighborIndex, dimnames,BiocNeighborIndex
#'          dim,BiocNeighborIndex
NULL

#' @exportMethod show
#' @importFrom methods show 
#' @rdname BiocNeighborIndex
#' @aliases show,BiocNeighborIndex-method
setMethod("show", "BiocNeighborIndex", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("dim: %i %i\n", nrow(object), ncol(object)))
    cat(sprintf("distance: %s\n", bndistance(object)))
})

#' @exportMethod bndistance
#' @rdname BiocNeighborIndex
#' @aliases bndistance,BiocNeighborIndex-method
setMethod("bndistance", "BiocNeighborIndex", function(x) x@distance)

#' @exportMethod dimnames
#' @rdname BiocNeighborIndex
#' @aliases dimnames,BiocNeighborIndex-method
setMethod("dimnames", "BiocNeighborIndex", function(x) {
    list(x@NAMES, NULL)
})

#' @exportMethod bndata
#' @rdname BiocNeighborIndex
#' @aliases bndata,BiocNeighborIndex-method
setMethod("bndata", "BiocNeighborIndex", function(x) x@data)

#' @exportMethod dim
#' @rdname BiocNeighborIndex
#' @aliases dim,BiocNeighborIndex-method
setMethod("dim", "BiocNeighborIndex", function(x) rev(dim(bndata(x))) ) # reversed, as matrix was transposed.

#' @importFrom S4Vectors setValidity2
setValidity2("BiocNeighborIndex", function(object) {
    msg <- character(0) 

    NAMES <- rownames(object)
    if (!is.null(NAMES) && length(NAMES)!=nrow(object)) {
        msg <- c(msg, "length of non-NULL 'NAMES' is not equal to the number of rows")
    }

    if (length(bndistance(object))!=1L) {
        msg <- c(msg, "'distance' must be a string")
    }
    
    if (length(msg)) return(msg)
    return(TRUE)
})
