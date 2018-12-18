#' @export
#' @importFrom BiocParallel SerialParam 
findKmknn <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_find_exact(X, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=.find_kmknn, searchArgsFUN=.find_kmknn_args, ...) 
}

.find_kmknn <- function(jobs, data, centers, info, distance, k, get.index, get.distance) {
    .Call(cxx_find_kmknn, jobs, data, centers, info, distance, k, get.index, get.distance)
}

.find_kmknn_args <- function(precomputed) {
    list(
        centers=KmknnIndex_cluster_centers(precomputed),
        info=KmknnIndex_cluster_info(precomputed), 
        distance="Euclidean"
    )
}
