####################
# Default dispatch #
####################

#' @export
setMethod("queryNeighbors", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("queryNeighbors", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    rangeQueryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryNeighbors", c("missing", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(rangeQueryKmknn, c(list(..., distance=bndistance(BNPARAM)), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("queryNeighbors", c("KmknnIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    rangeQueryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryNeighbors", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeQueryVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryNeighbors", c("missing", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeQueryVptree(..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryNeighbors", c("VptreeIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    rangeQueryVptree(..., precomputed=BNINDEX)
})
