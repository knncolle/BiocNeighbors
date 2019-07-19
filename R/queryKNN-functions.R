#########
# Annoy #
#########

#' @export
#' @importFrom BiocParallel SerialParam 
queryAnnoy <- function(X, query, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_approx(X, query, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        buildFUN=buildAnnoy, pathFUN=AnnoyIndex_path, searchFUN=query_annoy, 
        searchArgsFUN=.find_annoy_args, distFUN=query_dist_to_annoy, ...)
}

########
# HNSW #
########

#' @export
#' @importFrom BiocParallel SerialParam 
queryHnsw <- function(X, query, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_approx(X, query, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        buildFUN=buildHnsw, pathFUN=HnswIndex_path, searchFUN=query_hnsw, 
        searchArgsFUN=.find_hnsw_args, distFUN=query_dist_to_hnsw, ...)
}

#########
# KMKNN #
#########

#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryKmknn <- function(X, query, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_exact(X, query, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=query_kmknn, searchArgsFUN=.find_kmknn_args, 
        distFUN=query_dist_to_kmknn, ...) 
}

###########
# VP-tree #
###########

#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryVptree <- function(X, query, k, get.index=TRUE, get.distance=TRUE, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 2 December 2018
{
    .template_query_exact(X, query, k, get.index=get.index, get.distance=get.distance, 
        BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=query_vptree, searchArgsFUN=.find_vptree_args, 
        distFUN=query_dist_to_vptree, ...)
}
