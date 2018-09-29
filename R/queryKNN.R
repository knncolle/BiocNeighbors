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
    do.call(queryKmknn, c(list(...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("queryKNN", c("KmknnIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryKmknn(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("AnnoyIndex", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., precomputed=BNINDEX)
})

#' @export
setMethod("queryKNN", c("NULL", "AnnoyParam"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM))
})

#' @export
setMethod("queryKNN", c("AnnoyIndex", "NULL"), function(..., BNINDEX, BNPARAM) {
    queryAnnoy(..., precomputed=BNINDEX)
})
