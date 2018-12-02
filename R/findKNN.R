####################
# Further dispatch #
####################

#' @export
setMethod("findKNN", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("missing", "BiocNeighborParam"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("BiocNeighborIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("NULL", "missing"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("missing", "NULL"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("NULL", "NULL"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNINDEX=BNINDEX, BNPARAM=KmknnParam())        
})

####################
# Specific methods #
####################

#' @export
setMethod("findKNN", c("NULL", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(findKmknn, c(list(...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("findKNN", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    findKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("KmknnIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    findKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("NULL", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    findVptree(...)
})

#' @export
setMethod("findKNN", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    findVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("VptreeIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    findVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("NULL", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    findAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM))
})

#' @export
setMethod("findKNN", c("AnnoyIndex", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    findAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("AnnoyIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    findAnnoy(..., precomputed=BNINDEX)
})
