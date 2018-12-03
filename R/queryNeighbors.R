####################
# Further dispatch #
####################

#' @export
setMethod("queryNeighbors", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryNeighbors", c("missing", "BiocNeighborParam"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryNeighbors", c("BiocNeighborIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryNeighbors", c("NULL", "missing"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryNeighbors", c("missing", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryNeighbors", c("NULL", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryNeighbors(..., BNINDEX=BNINDEX, BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("queryNeighbors", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    rangeQueryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryNeighbors", c("NULL", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(rangeQueryKmknn, c(list(...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("queryNeighbors", c("KmknnIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    rangeQueryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryNeighbors", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeQueryVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryNeighbors", c("NULL", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeQueryVptree(...)
})

#' @export
setMethod("queryNeighbors", c("VptreeIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    rangeQueryVptree(..., precomputed=BNINDEX)
})
