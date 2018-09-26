#' @export
setMethod("buildNNIndex", c("ANY", "missing"), function(X, ..., BNPARAM) {
    buildNNIndex(X, ..., BNPARAM=BNPARAM)
})

#' @export
setMethod("buildNNIndex", c("ANY", "KmknnParam"), function(X, ..., BNPARAM) {
    do.call(buildKmknn, c(list(X=X, ...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("buildNNIndex", c("ANY", "AnnoyParam"), function(X, ..., BNPARAM) {
    buildAnnoy(X, ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM), ...)
})
