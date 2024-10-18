#' The BiocNeighborIndex class
#'
#' A virtual class for indexing structures of different nearest-neighbor search algorithms.
#' Developers should define subclasses for their own \code{\link{buildIndex}} and/or \code{\link{defineBuilder}} methods.
#'
#' @details
#' In general, the internal structure of a BiocNeighborIndex class is arbitrary and left to the discretion of the developer.
#' If an arbitrary structure is used, the associated methods should be written for all downstream generics like \code{\link{findKNN}}, etc.
#'
#' Alternatively, developers may choose to derive from the BiocNeighborGenericIndex class.
#' This expects:
#' \itemize{
#' \item A \code{ptr} slot containing an external pointer that refers to a \code{BiocNeighbors::Prebuilt} object
#' (see definition in \code{system.file("include", "BiocNeighbors.h", package="BiocNeighbors")}).
#' \item A \code{names} slot containing a character vector with the names of the observations, or \code{NULL} if no names are available.
#' This is used by \code{subset=} in the various \code{find*} generics.
#' }
#' In this case, no additional methods are required for the downstream generics.
#'
#' @author
#' Aaron Lun
#'
#' @aliases
#' BiocNeighborIndex-class
#' show,BiocNeighborIndex-method
#' BiocNeighborGenericIndex-class
#' @name BiocNeighborIndex
NULL

#' @export
#' @importFrom methods show
setMethod("show", "BiocNeighborIndex", function(object) {
    cat(sprintf("class: %s\n", class(object)))
})

