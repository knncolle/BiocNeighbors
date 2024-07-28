#' @export
#' @rdname buildIndex
setGeneric("buildIndex", signature=c("X", "BNPARAM"), function(X, transposed=FALSE, ..., BNPARAM=NULL) standardGeneric("buildIndex"))

# This is explicitly a S4 generic so that developers can extend it at the R
# level, not at the C++ level. We need to support dispatch on both X and
# BNPARAM as X could be an arbitrary index structure (i.e., not an external
# pointer). If we only dispatched on BNPARAM, the developer wouldn't be able to
# select the correct method based on the type of a prebuilt non-pointer index
# without also being supplied the BNPARAM, which is not ergonomic for the user.

#' @export
#' @rdname findKNN
setGeneric("findKNN", signature=c("X", "BNPARAM"), function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BNPARAM=NULL) {
    standardGeneric("findKNN")
})

#' @export
#' @rdname queryKNN
setGeneric("queryKNN", signature=c("X", "BNPARAM"), function(X, query, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BNPARAM=NULL) {
    standardGeneric("queryKNN")
})

#' @export
#' @rdname findNeighbors
setGeneric("findNeighbors", signature=c("X", "BNPARAM"), function(X, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BNPARAM=NULL) {
    standardGeneric("findNeighbors")
})

#' @export
#' @rdname queryNeighbors
setGeneric("queryNeighbors", signature=c("X", "BNPARAM"), function(X, query, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BNPARAM=NULL) {
    standardGeneric("queryNeighbors")
})
