#' The BiocNeighborIndex class
#'
#' A virtual class for indexing structures of different nearest-neighbor search algorithms.
#' Developers should define subclasses for their own \code{\link{buildIndex}} and/or \code{\link{defineBuilder}} methods.
#' Subclasses containing a \code{ptr} slot will work with many of the default methods in \pkg{BiocNeighbors},
#' see \code{\link{buildIndex}} for details.
#'
#' @author
#' Aaron Lun
#'
#' @aliases
#' BiocNeighborIndex-class
#' show,BiocNeighborIndex-method
#' @name BiocNeighborIndex
NULL

#' @export
#' @importFrom methods show
setMethod("show", "BiocNeighborIndex", function(object) {
    cat(sprintf("class: %s\n", class(object)))
})

