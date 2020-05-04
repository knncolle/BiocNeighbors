#' Find k-nearest neighbors
#'
#' Find the k-nearest neighbors for each point in a data set, using exact or approximate algorithms.
#'
#' @param X A numeric data matrix where rows are points and columns are dimensions.
#' This can be missing if \code{BNINDEX} is supplied.
#' @param k An integer scalar for the number of nearest neighbors.
#' @param ... Further arguments to pass to individual methods.
#' This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance}, \code{last},
#' \code{warn.ties}, \code{raw.index} and \code{BPPARAM}.
#' See \code{?"\link{findKNN-methods}"} for more details.
#' @param BNINDEX A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
#' This can be missing if \code{BNPARAM} is supplied, see Details.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
#' This can be missing if \code{BNINDEX} is supplied, see Details.
#'
#' @return List containing:
#' \code{index}, an integer matrix of neighbor identities;
#' and \code{distance}, a numeric matrix of distances to those neighbors.
#' See \code{?"\link{findKNN-methods}"} for more details.
#'
#' @details
#' The class of \code{BNINDEX} and \code{BNPARAM} will determine dispatch to specific methods.
#' Only one of these arguments needs to be defined to resolve dispatch.
#' However, if both are defined, they cannot specify different algorithms.
#' 
#' If \code{BNINDEX} is supplied, \code{X} does not need to be specified.
#' In fact, any value of \code{X} will be ignored as all necessary information for the search is already present in \code{BNINDEX}.
#' Similarly, any parameters in \code{BNPARAM} will be ignored.
#' 
#' If both \code{BNINDEX} and \code{BNPARAM} are missing, the function will default to the KMKNN algorithm by setting \code{BNPARAM=KmknnParam()}.
#' 
#' @author Aaron Lun
#' @seealso
#' \code{\link{findExhaustive}},
#' \code{\link{findKmknn}},
#' \code{\link{findVptree}},
#' \code{\link{findAnnoy}}
#' and \code{\link{findHnsw}} for specific methods.
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' str(k.out <- findKNN(Y, k=10))
#' str(a.out <- findKNN(Y, k=10, BNPARAM=AnnoyParam()))
#' 
#' e.dex <- buildExhaustive(Y)
#' str(k.out2 <- findKNN(Y, k=10, BNINDEX=e.dex))
#' str(k.out3 <- findKNN(Y, k=10, BNINDEX=e.dex, BNPARAM=ExhaustiveParam()))
#' 
#' k.dex <- buildKmknn(Y)
#' str(k.out2 <- findKNN(Y, k=10, BNINDEX=k.dex))
#' str(k.out3 <- findKNN(Y, k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))
#' 
#' a.dex <- buildAnnoy(Y)
#' str(a.out2 <- findKNN(Y, k=10, BNINDEX=a.dex))
#' str(a.out3 <- findKNN(Y, k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
#' 
#' @name findKNN
#' @docType methods
NULL

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
