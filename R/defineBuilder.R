#' Define an index builder
#'
#' Define a builder object that can construct indices for nearest-neighbor searching with different algorithms.
#' 
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
#' If \code{NULL}, this defaults to a \linkS4class{KmknnParam} object. 
#' 
#' @return
#' List containing:
#' \itemize{
#' \item \code{builder}, a pointer to a builder instance that can be used to construct a prebuilt index in \code{\link{buildIndex}}.
#' \item \code{class}, the constructor for a \linkS4class{BiocNeighborIndex} subclass that accepts a \code{ptr} argument.
#' }
#' 
#' @details
#' The external pointer returned in \code{builder} should refer to a \code{BiocNeighbors::Builder} object,
#' see the definition in \code{system.file("include", "BiocNeighbors.h", package="BiocNeighbors")} for details.
#' If a developer defines a \code{defineBuilder} method, they do not have to define a new \code{\link{buildIndex}} method;
#' the existing \code{buildIndex} methods will automatically create the appropriate \linkS4class{BiocNeighborIndex} subclass based on \code{class}.
#'
#' Note that the pointer returned by \code{defineBuilder} should \emph{not} be used as the \code{ptr} in some of the \linkS4class{BiocNeighborIndex} subclasses.
#' The \code{ptr} slot is expected to contain a pointer referring to a \code{BiocNeighbors::Prebuilt} object, as returned by the default \code{\link{buildIndex}}.
#' Using the pointer from \code{builder} will probably crash the R session.
#'
#' Needless to say, users should not attempt to serialize the external pointer returned by this generic.
#' Attempting to use a deserialized pointer in \code{\link{buildIndex}} will cause the R session to crash.
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
