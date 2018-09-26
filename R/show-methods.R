#' @importFrom methods show 
setMethod("show", "BiocNeighborParam", function(object) {
    cat(sprintf("class: %s\n", class(object)))
})

setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", AnnoyParam_ntrees(object)))
    cat(sprintf("directory: %s\n", AnnoyParam_directory(object)))
})

setMethod("show", "BiocNeighborIndex", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("dim: %i %i\n", nrow(object), ncol(object)))
})

setMethod("show", "KmknnIndex", function(object) {
    callNextMethod()
    cat(sprintf("clusters: %i\n", ncol(KmknnIndex_cluster_centers(object))))
})

setMethod("show", "AnnoyIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", AnnoyIndex_path(object)))
})
