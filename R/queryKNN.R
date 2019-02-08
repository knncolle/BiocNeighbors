####################
# Default dispatch #
####################

#' @export
setMethod("queryKNN", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("queryKNN", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    queryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("missing", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(queryKmknn, c(list(..., distance=bndistance(BNPARAM)), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("queryKNN", c("KmknnIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    queryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    queryVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("missing", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    queryVptree(..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryKNN", c("VptreeIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    queryVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("AnnoyIndex", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("missing", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM), 
        search.mult=AnnoyParam_search_mult(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryKNN", c("AnnoyIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("HnswIndex", "HnswParam"), function(..., BNINDEX, BNPARAM) {
    queryHnsw(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("missing", "HnswParam"), function(..., BNINDEX, BNPARAM) {
    queryHnsw(..., nlinks=HnswParam_nlinks(BNPARAM), ef.construction=HnswParam_ef_construction(BNPARAM),
        directory=HnswParam_directory(BNPARAM), ef.search=HnswParam_ef_search(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryKNN", c("HnswIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    queryHnsw(..., precomputed=BNINDEX)
})
