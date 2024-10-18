#' Build a nearest-neighbor index
#'
#' Build indices for nearest-neighbor searching with different algorithms.
#' 
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param ... Further arguments to be passed to individual methods.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
#' If \code{NULL}, this defaults to a \linkS4class{KmknnParam} object. 
#' 
#' Alternatively, this may be a list returned by \code{\link{defineBuilder}}.
#' 
#' @return
#' A \linkS4class{BiocNeighborIndex} object can be used in \code{\link{findKNN}} and related functions as the \code{X=} argument.
#' Users should assume that the index is not serializable, i.e., cannot be saved or transferred between processes.
#'
#' @details
#' Each \code{buildIndex} method is expected to return an instance of a \linkS4class{BiocNeighborIndex} subclass.
#' The structure of this subclass is arbitrary and left to the discretion of the method developer.
#' Developers are also responsible for defining methods for their subclass in each of the relevant functions (e.g., \code{\link{findKNN}}, \code{\link{queryKNN}}).
#' The exception is if the subclass contains a \code{ptr} slot that refers to a \code{BiocNeighbors::Prebuilt} object
#' (see definition in \code{system.file("include", "BiocNeighbors.h", package="BiocNeighbors")}).
#' This allows it to be directly used with the existing default methods for \code{\link{findKNN}}, etc.
#' 
#' @author
#' Aaron Lun
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' (k.out <- buildIndex(Y))
#' (a.out <- buildIndex(Y, BNPARAM=AnnoyParam()))
#'
#' @aliases
#' buildIndex,matrix,NULL-method
#' buildIndex,matrix,missing-method
#' buildIndex,matrix,BiocNeighborParam-method
#' buildIndex,matrix,list-method
#'
#' @name buildIndex
NULL

#' @export
setMethod("buildIndex", c("matrix", "missing"), function(X, transposed=FALSE, ..., BNPARAM) callGeneric(X, transposed=transposed, ..., BNPARAM=NULL))

#' @export
setMethod("buildIndex", c("matrix", "NULL"), function(X, transposed=FALSE, ..., BNPARAM) callGeneric(X, transposed=transposed, ..., BNPARAM=KmknnParam()))

#' @export
setMethod("buildIndex", c("matrix", "BiocNeighborParam"), function(X, transposed=FALSE, ..., BNPARAM) callGeneric(X, transposed=transposed, ..., BNPARAM=defineBuilder(BNPARAM)))

#' @export
setMethod("buildIndex", c("matrix", "list"), function(X, transposed=FALSE, ..., BNPARAM) {
    X <- .coerce_matrix_build(X, transposed)
    BNPARAM$class(ptr=generic_build(BNPARAM$builder, X), names=colnames(X))
})
