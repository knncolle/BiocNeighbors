#' @export
#' @rdname buildIndex
setGeneric("buildIndex", signature=c("BNPARAM"), function(X, ..., BNPARAM) standardGeneric("buildIndex"))

#' @export
#' @rdname findKNN-methods
setGeneric("findKNN", signature=c("X", "BNPARAM"), function(X, k, ..., BNPARAM) standardGeneric("findKNN"))

#' @export
#' @rdname queryKNN-methods
setGeneric("queryKNN", signature=c("X", "BNPARAM"), function(X, query, k, ..., BNPARAM) standardGeneric("queryKNN"))

#' @export
#' @rdname findNeighbors-methods
setGeneric("findNeighbors", signature=c("X", "BNPARAM"), function(X, threshold, ..., BNPARAM) standardGeneric("findNeighbors"))

#' @export
#' @rdname queryNeighbors-methods
setGeneric("queryNeighbors", signature=c("X", "BNPARAM"), function(X, query, threshold, ..., BNINDEX, BNPARAM) standardGeneric("queryNeighbors"))

#' @export
setGeneric("bndistance", function(x) standardGeneric("bndistance"))
