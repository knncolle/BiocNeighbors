#' @export
setMethod("buildNNIndex", c("ANY", "KmknnParam"), function(X, ..., BNPARAM) {
    out <- buildKmknn(X, ...)
    BNPARAM@precomputed <- out
    BNPARAM
})
