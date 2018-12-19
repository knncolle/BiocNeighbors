####################
# Further dispatch #
####################

#' @export
setMethod("buildIndex", "missing", function(..., BNPARAM) {
    buildIndex(..., BNPARAM=BNPARAM)
})

#' @export
setMethod("buildIndex", "NULL", function(..., BNPARAM) {
    buildIndex(..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("buildIndex", "KmknnParam", function(..., BNPARAM) {
    do.call(buildKmknn, c(list(..., distance=bndistance(BNPARAM)), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("buildIndex", "VptreeParam", function(..., BNPARAM) {
    buildVptree(..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("buildIndex", "AnnoyParam", function(..., BNPARAM) {
    buildAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("buildIndex", "HnswParam", function(..., BNPARAM) {
    buildHnsw(..., nlinks=HnswParam_nlinks(BNPARAM), ef.construction=HnswParam_ef_construction(BNPARAM), 
        directory=HnswParam_directory(BNPARAM), distance=bndistance(BNPARAM))
})
