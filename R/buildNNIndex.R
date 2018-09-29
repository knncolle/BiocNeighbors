#' @export
setMethod("buildNNIndex", "missing", function(..., BNPARAM) {
    buildNNIndex(..., BNPARAM=BNPARAM)
})

#' @export
setMethod("buildNNIndex", "NULL", function(..., BNPARAM) {
    buildNNIndex(..., BNPARAM=KmknnParam())
})

#' @export
setMethod("buildNNIndex", "KmknnParam", function(..., BNPARAM) {
    do.call(buildKmknn, c(list(...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("buildNNIndex", "AnnoyParam", function(..., BNPARAM) {
    buildAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM))
})
