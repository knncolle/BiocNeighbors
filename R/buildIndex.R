####################
# Default dispatch #
####################

#' @export
setMethod("buildIndex", "missing", function(X, transposed=FALSE, ..., BNPARAM) {
    buildIndex(X=X, transposed=transposed, ..., BNPARAM=KmknnParam())
})

####################
# Specific methods #
####################

#' @export
setMethod("buildIndex", "KmknnParam", function(X, transposed=FALSE, ..., BNPARAM) {
    do.call(buildKmknn, 
        c(
            list(
                X=X, transposed=transposed, ..., 
                distance=bndistance(BNPARAM)
            ), 
            KmknnParam_kmeans_args(BNPARAM)
        )
    )
})

#' @export
setMethod("buildIndex", "VptreeParam", function(X, transposed=FALSE, ..., BNPARAM) {
    buildVptree(X=X, transposed=transposed, ..., distance=bndistance(BNPARAM))
})

#' @export
setMethod("buildIndex", "AnnoyParam", function(X, transposed=FALSE, ..., BNPARAM) {
    buildAnnoy(X=X, transposed=transposed, ..., 
        ntrees=AnnoyParam_ntrees(BNPARAM), directory=AnnoyParam_directory(BNPARAM), 
        search.mult=AnnoyParam_search_mult(BNPARAM), distance=bndistance(BNPARAM))
})

#' @export
setMethod("buildIndex", "HnswParam", function(X, transposed=FALSE, ..., BNPARAM) {
    buildHnsw(X=X, transposed=transposed, ..., 
        nlinks=HnswParam_nlinks(BNPARAM), ef.construction=HnswParam_ef_construction(BNPARAM), 
        directory=HnswParam_directory(BNPARAM), ef.search=HnswParam_ef_search(BNPARAM), distance=bndistance(BNPARAM))
})
