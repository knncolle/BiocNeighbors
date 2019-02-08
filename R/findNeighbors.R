####################
# Default dispatch #
####################

#' @export
setMethod("findNeighbors", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("findNeighbors", c("missing", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(rangeFindKmknn, c(list(..., distance=bndistance(BNPARAM)), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("findNeighbors", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    rangeFindKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findNeighbors", c("KmknnIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    rangeFindKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findNeighbors", c("missing", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeFindVptree(..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("findNeighbors", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeFindVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("findNeighbors", c("VptreeIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    rangeFindVptree(..., precomputed=BNINDEX)
})
