# Defines common generics for all BiocNeighborParam classes.

setGeneric("buildNNIndex", signature=c("X", "BNPARAM"), function(X, ..., BNPARAM) standardGeneric("buildIndex"))

setGeneric("findKNN", signature=c("X", "BNPARAM"), function(X, ..., BNPARAM) standardGeneric("findKNN"))

setGeneric("queryKNN", signature=c("X", "BNPARAM"), function(X, ..., BNPARAM) standardGeneric("queryKNN"))
