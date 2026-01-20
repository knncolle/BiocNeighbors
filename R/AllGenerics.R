#' @export
#' @rdname buildIndex
setGeneric("buildIndex", signature="BNPARAM", function(X, transposed=FALSE, ..., BNPARAM=NULL) standardGeneric("buildIndex"))

#' @export
#' @rdname defineBuilder 
setGeneric("defineBuilder", signature="BNPARAM", function(BNPARAM) standardGeneric("defineBuilder"))

# This is explicitly a S4 generic so that developers can extend it at the R
# level, not at the C++ level. We need to support dispatch on both X and
# BNPARAM as X could be an arbitrary index structure (i.e., not an external
# pointer). If we only dispatched on BNPARAM, a user could call the method with
# a prebuilt X that doesn't match the BNPARAM. This means that the developer of
# the BNPARAM method would be responsible for figuring out what to do with a X
# that they don't know anything about, which is pretty weird.

#' @export
#' @rdname findKNN
setGeneric("findKnnFromIndex", signature="BNINDEX", function(BNINDEX, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ...) {
    standardGeneric("findKnnFromIndex")
})

#' @export
#' @rdname queryKNN
setGeneric("queryKnnFromIndex", signature="BNINDEX", function(BNINDEX, query, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ...) {
    standardGeneric("queryKnnFromIndex")
})

#' @export
#' @rdname findNeighbors
setGeneric("findNeighborsFromIndex", signature="BNINDEX", function(BNINDEX, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ...) {
    standardGeneric("findNeighborsFromIndex")
})

#' @export
#' @rdname queryNeighbors
setGeneric("queryNeighborsFromIndex", signature="BNINDEX", function(BNINDEX, query, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ...) {
    standardGeneric("queryNeighborsFromIndex")
})

#' @export
#' @rdname findDistance
setGeneric("findDistanceFromIndex", signature="BNINDEX", function(BNINDEX, k, num.threads=1, subset=NULL, ...) {
    standardGeneric("findDistanceFromIndex")
})

#' @export
#' @rdname queryDistance
setGeneric("queryDistanceFromIndex", signature="BNINDEX", function(BNINDEX, query, k, num.threads=1, subset=NULL, transposed=FALSE, ...) {
    standardGeneric("queryDistanceFromIndex")
})
