#' @export
setMethod("findKNN", c("ANY", "KmknnParam"), function(X, ..., BNPARAM) {
    findKmknn(X=X, k=k, precomputed=BNPARAM@precomputed, raw.index=BPPARAM@raw.index, ...)
})
