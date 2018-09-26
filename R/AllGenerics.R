# Defines common generics for all BiocNeighborParam classes.

setGeneric("buildNNIndex", signature=c("X", "BNPARAM"), function(X, ..., BNPARAM) standardGeneric("buildNNIndex"))

setGeneric("findKNN", signature=c("X", "BNINDEX", "BNPARAM"), function(X, ..., BNINDEX, BNPARAM) standardGeneric("findKNN"))

setGeneric("queryKNN", signature=c("X", "BNPARAM"), function(X, ..., BNPARAM) standardGeneric("queryKNN"))
