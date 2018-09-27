# Defines common generics for all BiocNeighborParam classes.

#' @export
setGeneric("buildNNIndex", signature=c("BNPARAM"), function(..., BNPARAM=NULL) standardGeneric("buildNNIndex"))

#' @export
setGeneric("findKNN", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX=NULL, BNPARAM=NULL) standardGeneric("findKNN"))

#' @export
setGeneric("queryKNN", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX=NULL, BNPARAM=NULL) standardGeneric("queryKNN"))
