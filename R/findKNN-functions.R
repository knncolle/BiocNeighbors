#########
# Annoy #
#########

#' @export
#' @importFrom BiocParallel SerialParam 
findAnnoy <- function(X, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 25 September June 2018
{
    .template_find_approx(X, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        buildFUN=buildAnnoy, pathFUN=AnnoyIndex_path, searchFUN=find_annoy, 
        searchArgsFUN=.find_annoy_args, distFUN=find_dist_to_annoy, ...) 
}

.find_annoy_args <- function(precomputed) {
    list(
        ndims=ncol(precomputed),
        fname=AnnoyIndex_path(precomputed),
        search_mult=AnnoyIndex_search_mult(precomputed)
    )
}

########
# HNSW #
########

#' @export
#' @importFrom BiocParallel SerialParam
findHnsw <- function(X, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, ...)
# Find nearest neighbors using the Hnsw approximate nearest neighbors algorithm.
# 
# written by Aaron Lun
# created 14 December 2018
{
    .template_find_approx(X, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        buildFUN=buildHnsw, pathFUN=HnswIndex_path, searchFUN=find_hnsw, 
        searchArgsFUN=.find_hnsw_args, distFUN=find_dist_to_hnsw, ...)
}

.find_hnsw_args <- function(precomputed) {
    list(
        vals=bndata(precomputed),
        fname=HnswIndex_path(precomputed),
        ef_search=HnswIndex_ef_search(precomputed)
    )
}

#########
# KMKNN #
#########

#' @export
#' @importFrom BiocParallel SerialParam 
findKmknn <- function(X, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_find_exact(X, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=find_kmknn, searchArgsFUN=.find_kmknn_args, 
        distFUN=find_dist_to_kmknn, ...) 
}

.find_kmknn_args <- function(precomputed) {
    list(
        clust_centers=KmknnIndex_cluster_centers(precomputed),
        clust_info=KmknnIndex_cluster_info(precomputed)
    )
}

###########
# VP-tree #
###########

#' @export
#' @importFrom BiocParallel SerialParam 
findVptree <- function(X, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_find_exact(X, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=find_vptree, searchArgsFUN=.find_vptree_args, 
        distFUN=find_dist_to_vptree, ...)
}

.find_vptree_args <- function(precomputed) {
    list(
        nodes=VptreeIndex_nodes(precomputed)
    )
}
