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
    queryKmknn(X=X, ...)
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
    queryAnnoy(X=X, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "AnnoyIndex", "NULL"), function(X, ..., BNINDEX, BNPARAM) {
    queryAnnoy(X=X, precomputed=BNINDEX, ...)
})
