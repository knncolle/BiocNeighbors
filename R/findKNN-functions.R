#########
# Annoy #
#########

#' @export
#' @importFrom BiocParallel SerialParam 
findAnnoy <- function(X, k, get.index=TRUE, get.distance=TRUE, last=k,
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=NA, warn.ties=NA, ...)
# Identifies nearest neighbours with the Annoy algorithm.
#
# written by Aaron Lun
# created 25 September June 2018
{
    .template_find_knn(X, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        exact=FALSE, warn.ties=FALSE, raw.index=FALSE,
        buildFUN=buildAnnoy, pathFUN=AnnoyIndex_path, searchFUN=find_annoy, searchArgsFUN=.find_annoy_args, ...)
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
findHnsw <- function(X, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=NA, warn.ties=NA, ...)
# Find nearest neighbors using the Hnsw approximate nearest neighbors algorithm.
# 
# written by Aaron Lun
# created 14 December 2018
{
    .template_find_knn(X, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        exact=FALSE, warn.ties=FALSE, raw.index=FALSE,
        buildFUN=buildHnsw, pathFUN=HnswIndex_path, searchFUN=find_hnsw, searchArgsFUN=.find_hnsw_args, ...)
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
findKmknn <- function(X, k, get.index=TRUE, get.distance=TRUE, last=k,
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, warn.ties=TRUE, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_find_knn(X, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=find_kmknn, searchArgsFUN=.find_kmknn_args, ...) 
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
findVptree <- function(X, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, warn.ties=TRUE, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_find_knn(X, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=find_vptree, searchArgsFUN=.find_vptree_args, ...)
}

.find_vptree_args <- function(precomputed) {
    list(
        nodes=VptreeIndex_nodes(precomputed)
    )
}

##############
# Exhaustive #
##############

#' @export
#' @importFrom BiocParallel SerialParam 
findExhaustive <- function(X, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, warn.ties=TRUE, ...)
# Identifies nearest neighbours with all versus all distance calculations.
#
# created April 12 2020 
{
    .template_find_knn(X, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildExhaustive, searchFUN=find_exhaustive, searchArgsFUN=.find_exhaustive_args, ...)
}

.find_exhaustive_args <- function(precomputed) {
    list()
}
