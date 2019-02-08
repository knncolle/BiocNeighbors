####################
# Default dispatch #
####################

#' @export
setMethod("findKNN", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    findKNN(..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("findKNN", c("missing", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(findKmknn, c(list(..., distance=bndistance(BNPARAM)), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("findKNN", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    findKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("KmknnIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    findKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("missing", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    findVptree(..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("findKNN", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    findVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("VptreeIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    findVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("missing", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    findAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM), 
        search.mult=AnnoyParam_search_mult(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("findKNN", c("AnnoyIndex", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    findAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("AnnoyIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    findAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("missing", "HnswParam"), function(..., BNINDEX, BNPARAM) {
    findHnsw(..., nlinks=HnswParam_nlinks(BNPARAM), ef.construction=HnswParam_ef_construction(BNPARAM), 
        directory=HnswParam_directory(BNPARAM), ef.search=HnswParam_ef_search(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("findKNN", c("HnswIndex", "HnswParam"), function(..., BNINDEX, BNPARAM) {
    findHnsw(..., precomputed=BNINDEX)
})

#' @export
setMethod("findKNN", c("HnswIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    findHnsw(..., precomputed=BNINDEX)
})
