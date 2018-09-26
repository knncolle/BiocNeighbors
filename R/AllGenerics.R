# Defines common generics for all BiocNeighborParam classes.

#' @export
setGeneric("buildNNIndex", signature=c("X", "BNPARAM"), function(X, ..., BNPARAM=KmknnParam()) standardGeneric("buildNNIndex"))

#' @export
setGeneric("findKNN", signature=c("X", "BNINDEX", "BNPARAM"), function(X, ..., BNINDEX=NULL, BNPARAM=KmknnParam()) standardGeneric("findKNN"))

#' @export
setGeneric("queryKNN", signature=c("X", "BNINDEX", "BNPARAM"), function(X, ..., BNINDEX=NULL, BNPARAM=KmknnParam()) standardGeneric("queryKNN"))
