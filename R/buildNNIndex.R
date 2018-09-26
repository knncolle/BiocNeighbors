#' @export
setMethod("buildNNIndex", c("ANY", "missing"), function(X, ..., BNPARAM) {
    buildKmknn(X, ..., BNPARAM=BNPARAM)
})

#' @export
setMethod("buildNNIndex", c("ANY", "KmknnParam"), function(X, ..., BNPARAM) {
    buildKmknn(X, ...)
})

#' @export
setMethod("buildNNIndex", c("ANY", "AnnoyParam"), function(X, fname=NULL, ..., BNPARAM) {
    dir <- AnnoyParam_directory(BNPARAM)
    if (is.null(fname)) {
        fname <- tempfile(tmpdir=dir, fileext=".idx")
    } else {
        fname <- file.path(dir, fname)
    }
    buildAnnoy(X, ntrees=AnnoyParam_ntrees(BNPARAM), fname=fname)
})
