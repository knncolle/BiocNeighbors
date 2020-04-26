#########
# Annoy #
#########

#' @export
#' @importFrom BiocParallel SerialParam 
queryAnnoy <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=NA, warn.ties=NA, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=FALSE, warn.ties=FALSE, raw.index=FALSE,
        buildFUN=buildAnnoy, pathFUN=AnnoyIndex_path, searchFUN=query_annoy, searchArgsFUN=.find_annoy_args, ...)
}

########
# HNSW #
########

#' @export
#' @importFrom BiocParallel SerialParam 
queryHnsw <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=NA, warn.ties=NA, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=FALSE, warn.ties=FALSE, raw.index=FALSE,
        buildFUN=buildHnsw, pathFUN=HnswIndex_path, searchFUN=query_hnsw, searchArgsFUN=.find_hnsw_args, ...)
}

#########
# KMKNN #
#########

#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryKmknn <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k,
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, 
    raw.index=FALSE, warn.ties=TRUE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset,
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=query_kmknn, searchArgsFUN=.find_kmknn_args, ...)
}

###########
# VP-tree #
###########

#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryVptree <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, 
    raw.index=FALSE, warn.ties=TRUE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 2 December 2018
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=query_vptree, searchArgsFUN=.find_vptree_args, ...)
}

##############
# Exhaustive #
##############

#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryExhaustive <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, 
    raw.index=FALSE, warn.ties=TRUE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# created 21 April 2020
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildExhaustive, searchFUN=query_exhaustive, searchArgsFUN=.find_exhaustive_args, ...)
}

