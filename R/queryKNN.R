#' @export
setMethod("queryKNN", c("ANY", "KmknnParam"), function(X, ..., BNPARAM) {
    queryKmknn(X=X. precomputed=BNPARAM@precomputed, raw.index=BPPARAM@raw.index, ...)
})
