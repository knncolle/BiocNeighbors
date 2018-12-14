#' @export
#' @importFrom methods show 
setMethod("show", "BiocNeighborParam", function(object) {
    cat(sprintf("class: %s\n", class(object)))
})

#' @export
setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", AnnoyParam_ntrees(object)))
    cat(sprintf("directory: %s\n", AnnoyParam_directory(object)))
})

#' @export
setMethod("show", "BiocNeighborIndex", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("dim: %i %i\n", nrow(object), ncol(object)))
})

#' @export
setMethod("show", "KmknnIndex", function(object) {
    callNextMethod()
    cat(sprintf("clusters: %i\n", ncol(KmknnIndex_cluster_centers(object))))
})

#' @export
setMethod("show", "AnnoyIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", AnnoyIndex_path(object)))
})

#' @export
setMethod("show", "HnswIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", HnswIndex_path(object)))
})
