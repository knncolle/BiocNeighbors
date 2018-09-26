#' @export
setMethod("findKNN", c("ANY", "missing", "missing"), function(X, ..., BNINDEX, BNPARAM) {
    findKNN(X, ..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("ANY", "missing", "BiocNeighborParam"), function(X, ..., BNINDEX, BNPARAM) {
    findKNN(X, ..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("ANY", "BiocNeighborIndex", "missing"), function(X, ..., BNINDEX, BNPARAM) {
    findKNN(X, ..., BNINDEX=BNINDEX, BNPARAM=BNPARAM)
})

#' @export
setMethod("findKNN", c("ANY", "NULL", "KmknnParam"), function(X, ..., BNINDEX, BNPARAM) {
    findKmknn(X=X, ...)
})

#' @export
setMethod("findKNN", c("ANY", "KmknnIndex", "KmknnParam"), function(X, ..., BNINDEX, BNPARAM) {
    findKmknn(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("findKNN", c("ANY", "KmknnIndex", "NULL"), function(X, ..., BNINDEX, BNPARAM) {
    findKmknn(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("findKNN", c("ANY", "NULL", "AnnoyParam"), function(X, ..., BNINDEX, BNPARAM) {
    findAnnoy(X=X, ...)
})

#' @export
setMethod("findKNN", c("ANY", "AnnoyIndex", "AnnoyParam"), function(X, ..., BNINDEX, BNPARAM) {
    findAnnoy(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("findKNN", c("ANY", "AnnoyIndex", "NULL"), function(X, ..., BNINDEX, BNPARAM) {
    findAnnoy(X=X, precomputed=BNINDEX, ...)
})
