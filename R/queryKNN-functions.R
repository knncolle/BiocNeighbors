#' Query k-nearest neighbors
#'
#' Find the k-nearest neighbors in one data set for each point in another query data set, using exact or approximate algorithms.
#'
#' @param X A numeric data matrix where rows are points and columns are dimensions.
#' This can be missing if \code{BNINDEX} is supplied.
#' @param query A numeric matrix of query points, containing different data points in the rows but the same number and ordering of dimensions in the columns.
#' @param k A positive integer scalar specifying the number of nearest neighbors.
#' @param ... Further arguments to pass to individual methods.
#' This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance}, \code{last},
#' \code{transposed}, \code{warn.ties}, \code{raw.index} and \code{BPPARAM}.
#' See \code{?"\link{queryKNN-methods}"} for more details.
#' @param BNINDEX A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
#' This can be missing if \code{BNPARAM} is supplied, see Details.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
#' This can be missing if \code{BNINDEX} is supplied, see Details.
#'
#' @return List containing:
#' \code{index}, an integer matrix of neighbor identities;
#' and \code{distance}, a numeric matrix of distances to those neighbors.
#' See \code{?"\link{queryKNN-methods}"} for more details.
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
#' \code{\link{queryExhaustive}},
#' \code{\link{queryKmknn}},
#' \code{\link{queryVptree}},
#' \code{\link{queryAnnoy}}
#' and \code{\link{queryHnsw}} for specific methods.
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(10000), ncol=20)
#'
#' str(k.out <- queryKNN(Y, Z, k=10))
#' str(a.out <- queryKNN(Y, Z, k=10, BNPARAM=AnnoyParam()))
#' 
#' e.dex <- buildExhaustive(Y)
#' str(e.out2 <- queryKNN(Y, Z, k=10, BNINDEX=e.dex))
#' str(e.out3 <- queryKNN(Y, Z, k=10, BNINDEX=e.dex, BNPARAM=ExhaustiveParam()))
#' 
#' k.dex <- buildKmknn(Y)
#' str(k.out2 <- queryKNN(Y, Z, k=10, BNINDEX=k.dex))
#' str(k.out3 <- queryKNN(Y, Z, k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))
#' 
#' a.dex <- buildAnnoy(Y)
#' str(a.out2 <- queryKNN(Y, Z, k=10, BNINDEX=a.dex))
#' str(a.out3 <- queryKNN(Y, Z, k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
#'
#' @name queryKNN
#' @docType
NULL

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

