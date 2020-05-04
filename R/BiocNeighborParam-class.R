#' The BiocNeighborParam class
#'
#' A virtual class for specifying the type of nearest-neighbor search algorithm and associated parameters.
#'
#' @details
#' The BiocNeighborParam class is a virtual base class on which other parameter objects are built.
#' There are currently 5 concrete subclasses:
#' \describe{
#'     \item{}{\code{\link{ExhaustiveParam}}: exact nearest-neighbor search with the exhaustive (i.e. brute-force) algorithm.}
#'     \item{}{\code{\link{KmknnParam}}: exact nearest-neighbor search with the KMKNN algorithm.}
#'     \item{}{\code{\link{VptreeParam}}: exact nearest-neighbor search with the VP tree algorithm.}
#'     \item{}{\code{\link{AnnoyParam}}: approximate nearest-neighbor search with the Annoy algorithm.}
#'     \item{}{\code{\link{HnswParam}}: approximate nearest-neighbor search with the HNSW algorithm.}
#' These objects hold parameters specifying how each algorithm should be run on an arbitrary data set.
#' See the associated documentation pages for more details.
#' }
#'
#' @seealso
#' \code{\link{ExhaustiveParam}},
#' \code{\link{KmknnParam}},
#' \code{\link{VptreeParam}},
#' \code{\link{AnnoyParam}},
#' and \code{\link{HnswParam}} for constructors.
#'
#' \code{\link{buildIndex}}, \code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
#' @author Aaron Lun
#' @docType class
#' @name BiocNeighborParam
#' @aliases BiocNeighborParam, BiocNeighborParam-class, bndistance,BiocNeighborParam-method, 
#'          show,BiocNeighborParam-method
NULL

#' Display the class of a BiocNeighborParam \code{object}.
#' @exportMethod show
#' @rdname BiocNeighborParam
#' @aliases show,BiocNeighborParam
#' @importFrom methods show 
setMethod("show", "BiocNeighborParam", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("distance: %s\n", bndistance(object)))
})

#' Return a string specifying the distance metric to be used for searching, usually \code{"Euclidean"} or \code{"Manhattan"}.
#' @exportMethod bndistance
#' @rdname BiocNeighborParam
#' @aliases bndistance,BiocNeighborParam
setMethod("bndistance", "BiocNeighborParam", function(x) x@distance)

#' @importFrom S4Vectors setValidity2
setValidity2("BiocNeighborParam", function(object) {
    msg <- character(0) 

    if (length(bndistance(object))!=1L) {
        msg <- c(msg, "'distance' must be a string")
    }
    
    if (length(msg)) return(msg)
    return(TRUE)
})
