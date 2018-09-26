#' @export
setMethod("queryKNN", c("ANY", "KmknnIndex", "KmknnParam"), function(X, ..., BNINDEX=NULL, BNPARAM) {
    queryKmknn(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "NULL", "KmknnParam"), function(X, ..., BNINDEX=NULL, BNPARAM) {
    queryKmknn(X=X, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "KmknnIndex", "NULL"), function(X, ..., BNINDEX=NULL, BNPARAM) {
    queryKmknn(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "AnnoyIndex", "AnnoyParam"), function(X, ..., BNINDEX=NULL, BNPARAM) {
    queryAnnoy(X=X, precomputed=BNINDEX, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "NULL", "AnnoyParam"), function(X, ..., BNINDEX=NULL, BNPARAM) {
    queryAnnoy(X=X, ...)
})

#' @export
setMethod("queryKNN", c("ANY", "AnnoyIndex", "NULL"), function(X, ..., BNINDEX=NULL, BNPARAM) {
    queryAnnoy(X=X, precomputed=BNINDEX, ...)
})
