####################
# Further dispatch #
####################

#' @export
setMethod("queryKNN", c("missing", "missing"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("missing", "BiocNeighborParam"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("BiocNeighborIndex", "missing"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("NULL", "missing"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("missing", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("NULL", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryKNN(..., BNINDEX=BNINDEX, BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("queryKNN", c("KmknnIndex", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    queryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("NULL", "KmknnParam"), function(..., BNINDEX, BNPARAM) {
    do.call(queryKmknn, c(list(..., distance=bndistance(BNPARAM)), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("queryKNN", c("KmknnIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("VptreeIndex", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    queryVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("NULL", "VptreeParam"), function(..., BNINDEX, BNPARAM) {
    queryVptree(..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryKNN", c("VptreeIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryVptree(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("AnnoyIndex", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("NULL", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryKNN", c("AnnoyIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("HnswIndex", "HnswParam"), function(..., BNINDEX, BNPARAM) {
    queryHnsw(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("NULL", "HnswParam"), function(..., BNINDEX, BNPARAM) {
    queryHnsw(..., nlinks=HnswParam_nlinks(BNPARAM), ef.construction=HnswParam_ef_construction(BNPARAM),
        directory=HnswParam_directory(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("queryKNN", c("HnswIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryHnsw(..., precomputed=BNINDEX)
})
