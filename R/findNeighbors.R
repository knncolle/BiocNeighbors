####################
# Further dispatch #
####################

#' @export
setMethod("findNeighbors", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findNeighbors", c("missing", "BiocNeighborParam"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findNeighbors", c("BiocNeighborIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findNeighbors", c("NULL", "missing"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findNeighbors", c("missing", "NULL"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findNeighbors", c("NULL", "NULL"), function(..., BNINDEX, BNPARAM) {
    findNeighbors(..., BNINDEX=BNINDEX, BNPARAM=KmknnParam())        
})

####################
# Specific methods #
####################

#' @export
setMethod("findNeighbors", c("NULL", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(rangeFindKmknn, c(list(...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("findNeighbors", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    rangeFindKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findNeighbors", c("KmknnIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    rangeFindKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findNeighbors", c("NULL", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeFindVptree(...)
})

#' @export
setMethod("findNeighbors", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    rangeFindVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("findNeighbors", c("VptreeIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    rangeFindVptree(..., precomputed=BNINDEX)
})
