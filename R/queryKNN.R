#' @export
setMethod("queryKNN", c("ANY", "missing", "missing"), function(X, ..., BNINDEX, BNPARAM) {
    queryKNN(X, ..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("ANY", "missing", "BiocNeighborParam"), function(X, ..., BNINDEX, BNPARAM) {
    queryKNN(X, ..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("ANY", "BiocNeighborIndex", "missing"), function(X, ..., BNINDEX, BNPARAM) {
    queryKNN(X, ..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("queryKNN", c("ANY", "KmknnIndex", "KmknnParam"), function(X, ..., BNINDEX, BNPARAM) {
    queryKmknn(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "NULL", "KmknnParam"), function(X, ..., BNINDEX, BNPARAM) {
    do.call(queryKmknn, c(list(X=X, ...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("queryKNN", c("ANY", "KmknnIndex", "NULL"), function(X, ..., BNINDEX, BNPARAM) {
    queryKmknn(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "AnnoyIndex", "AnnoyParam"), function(X, ..., BNINDEX, BNPARAM) {
    queryAnnoy(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "NULL", "AnnoyParam"), function(X, ..., BNINDEX, BNPARAM) {
    queryAnnoy(X=X, ..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM))
})

#' @export
setMethod("queryKNN", c("ANY", "AnnoyIndex", "NULL"), function(X, ..., BNINDEX, BNPARAM) {
    queryAnnoy(X=X, precomputed=BNINDEX, ...)
})
