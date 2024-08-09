#' Define an index builder
#'
#' Define a builder object that can construct indices for nearest-neighbor searching with different algorithms.
#' 
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
#' If \code{NULL}, this defaults to a \linkS4class{KmknnParam} object. 
#' 
#' @return
#' Pointer to a builder instance that can be used to construct a prebuilt index in \code{\link{buildIndex}}.
#'
#' @details
#' Methods for this generic should return a pointer that refers to a \code{BiocNeighbors::Builder} object,
#' see definition in \code{system.file("include", "BiocNeighbors.h", package="BiocNeighbors")} for details.
#'
#' Note that the pointer returned by \code{defineBuilder} should \emph{not} be used in \code{\link{findKNN}}, \code{\link{queryKNN}}, etc.
#' Those methods instead accept the pointer returned by \code{\link{buildIndex}}.
#'
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{defineBuilder,KmknnParam-method}},
#' \code{\link{defineBuilder,VptreeParam-method}},
#' \code{\link{defineBuilder,AnnoyParam-method}} 
#' and \code{\link{defineBuilder,HnswParam-method}} for specific methods. 
#' 
#' @examples
#' (out <- defineBuilder())
#' (out2 <- defineBuilder(AnnoyParam()))
#'
#' @aliases
#' defineBuilder,NULL-method
#' defineBuilder,missing-method
#' @name defineBuilder 
NULL

#' @export
setMethod("defineBuilder", "NULL", function(BNPARAM) callGeneric(KmknnParam()))

#' @export
setMethod("defineBuilder", "missing", function(BNPARAM) callGeneric(NULL))
