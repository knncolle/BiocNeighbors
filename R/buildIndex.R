#' Build a nearest-neighbor index
#'
#' Build indices for nearest-neighbor searching with different algorithms.
#' 
#' @param X A numeric matrix or matrix-like object where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param ... Further arguments to be passed to individual methods.
#' If a method accepts arguments here, it should prefix these arguments with the algorithm name to avoid conflicts, e.g., \code{vptree.foo.bar}.
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
#' If \code{NULL} or missing, this defaults to a \linkS4class{KmknnParam} object. 
#' Alternatively, this may be a list returned by \code{\link{defineBuilder}}.
#' @param .check.nonfinite Boolean indicating whether to check for non-finite values in \code{X}.
#' This can be set to \code{FALSE} for greater efficiency.
#' 
#' 
#' @return
#' A \linkS4class{BiocNeighborIndex} object can be used in \code{\link{findKNN}} and related functions as the \code{X=} argument.
#' Users should assume that the index is not serializable, i.e., cannot be saved or transferred between processes.
#'
#' @details
#' Each \code{buildIndex} method is expected to return an instance of a \linkS4class{BiocNeighborIndex} subclass.
#' The structure of this subclass is arbitrary and left to the discretion of the method developer.
#' Developers are also responsible for defining methods for their subclass in each of the relevant functions (e.g., \code{\link{findKNN}}, \code{\link{queryKNN}}).
#' The exception is if the method returns an instance of a \linkS4class{BiocNeighborGenericIndex} subclass,
#' which can be used with the existing methods for \code{\link{findKNN}}, etc. without further effort.
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
#' buildIndex,NULL-method
#' buildIndex,missing-method
#' buildIndex,BiocNeighborParam-method
#' buildIndex,list-method
#'
#' @name buildIndex
NULL

#' @export
#' @rdname buildIndex
setMethod("buildIndex", "missing", function(X, BNPARAM, transposed=FALSE, ...) callGeneric(X, BNPARAM=NULL, transposed=transposed, ...))

#' @export
#' @rdname buildIndex
setMethod("buildIndex", "NULL", function(X, BNPARAM, transposed=FALSE, ...) callGeneric(X, BNPARAM=KmknnParam(), transposed=transposed, ...))

#' @export
#' @rdname buildIndex
setMethod("buildIndex", "BiocNeighborParam", function(X, BNPARAM, transposed=FALSE, ...) callGeneric(X, BNPARAM=defineBuilder(BNPARAM), transposed=transposed, ...))

#' @export
#' @rdname buildIndex
setMethod("buildIndex", "list", function(X, BNPARAM, transposed=FALSE, ..., .check.nonfinite=TRUE) {
    X <- .transpose_and_subset(X, transposed, subset=NULL)
    cn <- colnames(X)
    if (!is.matrix(X)) {
        X <- beachmat::initializeCpp(X)
    }
    BNPARAM$class(ptr=generic_build(BNPARAM$builder, X, .check.nonfinite), names=cn)
})
