####################
# Further dispatch #
####################

#' @export
setMethod("buildIndex", "missing", function(..., BNPARAM) {
    buildIndex(..., BNPARAM=BNPARAM)
})

#' @export
setMethod("buildIndex", "NULL", function(..., BNPARAM) {
    buildIndex(..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("buildIndex", "KmknnParam", function(..., BNPARAM) {
    do.call(buildKmknn, c(list(...), KmknnParam_kmeans_args(BNPARAM)))
})

#' @export
setMethod("buildIndex", "VptreeParam", function(..., BNPARAM) {
    buildVptree(...)
})

#' @export
setMethod("buildIndex", "AnnoyParam", function(..., BNPARAM) {
    buildAnnoy(..., ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM))
})
