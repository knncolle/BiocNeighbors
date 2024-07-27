#' The BiocNeighborParam class
#'
#' A virtual class for specifying the type of nearest-neighbor search algorithm and associated parameters.
#' 
#' @details
#' The BiocNeighborParam class is a virtual base class on which other parameter objects are built.
#' There are currently 4 concrete subclasses:
#' \describe{
#'     \item{}{\code{\link{KmknnParam}}: exact nearest-neighbor search with the KMKNN algorithm.}
#'     \item{}{\code{\link{VptreeParam}}: exact nearest-neighbor search with the VP tree algorithm.}
#'     \item{}{\code{\link{AnnoyParam}}: approximate nearest-neighbor search with the Annoy algorithm.}
#'     \item{}{\code{\link{HnswParam}}: approximate nearest-neighbor search with the HNSW algorithm.}
#' }
#' 
#' These objects hold parameters specifying how each algorithm should be run on an arbitrary data set.
#' See the associated documentation pages for more details.
#' 
#' @section Methods:
#' In the following code snippets, \code{x} and \code{object} are BiocNeighborParam objects.
#' \describe{
#' \item{\code{show(object)}:}{Display the class and arguments of \code{object}.}
#' \item{\code{bndistance(object)}:}{Return a string specifying the distance metric to be used for searching.
#' This should be one of \code{"Euclidean"}, \code{"Manhattan"} or \code{"Cosine"}.}
#' \item{\code{x[[i]]}:}{Return the value of slot \code{i}, as used in the constructor for \code{x}.}
#' \item{\code{x[[i]] <- value}:}{Set slot \code{i} to the specified \code{value}.}
#' }
#'
#' @section Distance metrics: 
#' All algorithms support neighbor searching by Euclidean, Manhattan and cosine distances.
#' Cosine distances are implemented as the Euclidean distance between L2-normalized vectors.
#' 
#' @seealso
#' \code{\link{KmknnParam}},
#' \code{\link{VptreeParam}},
#' \code{\link{AnnoyParam}},
#' and \code{\link{HnswParam}} for constructors.
#' 
#' \code{\link{buildIndex}}, \code{\link{findKNN}} and \code{\link{queryKNN}} for dispatch.
#' 
#' @author
#' Aaron Lun
#' 
#' @aliases
#' BiocNeighborParam-class
#' show,BiocNeighborParam-method
#' bndistance,BiocNeighborParam-method
#' [[,BiocNeighborParam-method
#' [[<-,BiocNeighborParam-method
#'
#' @name BiocNeighborParam
NULL

#' @export
#' @importFrom methods show 
setMethod("show", "BiocNeighborParam", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("distance: %s\n", bndistance(object)))
})

#' @export
bndistance <- function(x) x@distance

#' @importFrom S4Vectors setValidity2
setValidity2("BiocNeighborParam", function(object) {
    msg <- character(0) 

    if (length(bndistance(object))!=1L) {
        msg <- c(msg, "'distance' must be a string")
    }
    
    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
setMethod("[[", "BiocNeighborParam", function(x, i, j, ...) {
    # Provides a layer of protection that we can use to update
    # the object or intercept slot queries if the class changes.
    slot(x, i)
})

#' @export
setReplaceMethod("[[", "BiocNeighborParam", function(x, i, j, ..., value) {
    slot(x, i) <- value
    x
})
