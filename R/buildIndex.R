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
#' @return
#' A prebuilt index that can be used in \code{\link{findKNN}} and related functions as the \code{X=} argument.
#' The type and structure of this index is arbitrary and left to the discretion of the method developer;
#' however, users should assume that the index is not serializable (i.e., cannot be saved or transferred between processes).
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex,KmknnParam-method}},
#' \code{\link{buildIndex,VptreeParam-method}},
#' \code{\link{buildIndex,AnnoyParam-method}} 
#' and \code{\link{buildIndex,HnswParam-method}} for specific methods. 
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' (k.out <- buildIndex(Y))
#' (a.out <- buildIndex(Y, BNPARAM=AnnoyParam()))
#'
#' @aliases
#' buildIndex,matrix,NULL-method
#' buildIndex,matrix,missing-method
#'
#' @name buildIndex
NULL

#' @export
setMethod("buildIndex", c("matrix", "NULL"), function(X, transposed=FALSE, ..., BNPARAM) buildIndex(X, transposed=transposed, ..., BNPARAM=KmknnParam()))

#' @export
setMethod("buildIndex", c("matrix", "missing"), function(X, transposed=FALSE, ..., BNPARAM) buildIndex(X, transposed=transposed, ..., BNPARAM=KmknnParam()))
