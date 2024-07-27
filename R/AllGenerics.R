#' Build a nearest-neighbor index
#'
#' Build indices for nearest-neighbor searching with different algorithms.
#' 
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param ... Further arguments to be passed to individual methods.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
#' This defaults to a \linkS4class{KmknnParam} object if no argument is supplied.
#' 
#' @return
#' An external pointer that can be used in \code{\link{findKNN}} and related functions.
#' This is strictly for use within the same R session, as it cannot be serialized for use in other sessions or processes.
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
#' buildIndex,missing-method
#'
#' @export
setGeneric("buildIndex", signature=c("BNPARAM"), function(X, transposed=FALSE, ..., BNPARAM) standardGeneric("buildIndex"))
