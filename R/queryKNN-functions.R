#' Query nearest neighbors
#'
#' Query a dataset for nearest neighbors of points in another dataset, using a variety of algorithms.
#' 
#' @inheritParams findKNN-functions
#' @param query A numeric matrix of query points, containing different data points in the rows but the same number and ordering of dimensions in the columns.
#' @param last An integer scalar specifying the number of furthest neighbors for which statistics should be returned.
#' @param transposed A logical scalar indicating whether the \code{query} is transposed, 
#' in which case \code{query} is assumed to contain dimensions in the rows and data points in the columns.
#' @param subset A vector indicating the rows of \code{query} (or columns, if \code{transposed=TRUE}) for which the nearest neighbors should be identified.
#' 
#' @details
#' All of these functions identify points in \code{X} that are the \code{k} nearest neighbors of each point in \code{query}.
#' \code{queryAnnoy} performs an approximate search, while \code{queryExhaustive}, \code{queryKmknn} and \code{queryVptree} are exact.
#' This requires both \code{X} and \code{query} to have the same number of dimensions.
#' Moreover, the upper bound for \code{k} is set at the number of points in \code{X}.
#' 
#' By default, nearest neighbors are identified for all data points within \code{query}.
#' If \code{subset} is specified, nearest neighbors are only detected for the query points in the subset.
#' This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{queryKmknn} on the full \code{query}.
#' 
#' If \code{transposed=TRUE}, this function assumes that \code{query} is already transposed, which saves a bit of time by avoiding an unnecessary transposition.
#' Turning off \code{get.index} or \code{get.distance} may also provide a slight speed boost when these returned values are not of interest.
#' Using \code{BPPARAM} will also split the search by query points across multiple processes.
#' 
#' Setting \code{last} will return indices and/or distances for the \code{k - last + 1}-th closest neighbor to the \code{k}-th neighbor.
#' This can be used to improve memory efficiency, e.g., by only returning statistics for the \code{k}-th nearest neighbor by setting \code{last=1}.
#' Note that this is entirely orthogonal to \code{subset}.
#' 
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} (e.g., with \code{\link{buildKmknn}}).
#' The resulting BiocNeighborIndex object can be supplied as \code{precomputed} to multiple function calls, avoiding the need to repeat index construction in each call.
#' Note that when \code{precomputed} is supplied, the value of \code{X} is ignored.
#' 
#' For exact methods, see comments in \code{?"\link{BiocNeighbors-ties}"} regarding the warnings when tied distances are observed.
#' For approximate methods, see comments in \code{\link{buildAnnoy}} and \code{\link{buildHnsw}} about the (lack of) randomness in the search results.
#' 
#' @return
#' A list is returned containing:
#' \itemize{
#'     \item \code{index}, if \code{get.index=TRUE}.
#'     This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{query}.
#'     The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
#'     \item \code{distance}, if \code{get.distance=TRUE}.
#'     This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
#' }
#' Each matrix contains \code{last} columns.
#' If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.
#' 
#' See \code{?"\link{BiocNeighbors-raw-index}"} for an explanation of the output when \code{raw.index=TRUE} for the functions that support it.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildExhaustive}}, 
#' \code{\link{buildKmknn}}, 
#' \code{\link{buildVptree}},
#' or \code{\link{buildAnnoy}} to build an index ahead of time.
#' 
#' See \code{?"\link{BiocNeighbors-algorithms}"} for an overview of the available algorithms.
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(20000), ncol=20)
#' 
#' out <- queryExhaustive(Y, query=Z, k=5)
#' head(out$index)
#' head(out$distance)
#' 
#' out1 <- queryKmknn(Y, query=Z, k=5)
#' head(out1$index)
#' head(out1$distance)
#' 
#' out2 <- queryVptree(Y, query=Z, k=5)
#' head(out2$index)
#' head(out2$distance)
#' 
#' out3 <- queryAnnoy(Y, query=Z, k=5)
#' head(out3$index)
#' head(out3$distance)
#' 
#' out4 <- queryHnsw(Y, query=Z, k=5)
#' head(out4$index)
#' head(out4$distance)
#'
#' @name queryKNN-functions
NULL

#' @export
#' @rdname queryKNN-functions
#' @importFrom BiocParallel SerialParam 
queryAnnoy <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=NA, warn.ties=NA, ...)
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=FALSE, warn.ties=FALSE, raw.index=FALSE,
        buildFUN=buildAnnoy, searchFUN=query_annoy, searchArgsFUN=.find_annoy_args, ...)
}

#' @export
#' @rdname queryKNN-functions
#' @importFrom BiocParallel SerialParam 
queryHnsw <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=NA, warn.ties=NA, ...)
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=FALSE, warn.ties=FALSE, raw.index=FALSE,
        buildFUN=buildHnsw, searchFUN=query_hnsw, searchArgsFUN=.find_hnsw_args, ...)
}

#' @export
#' @rdname queryKNN-functions
#' @importFrom BiocParallel SerialParam bpmapply
queryKmknn <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k,
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, 
    raw.index=FALSE, warn.ties=TRUE, ...)
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset,
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=query_kmknn, searchArgsFUN=.find_kmknn_args, ...)
}

#' @export
#' @rdname queryKNN-functions
#' @importFrom BiocParallel SerialParam bpmapply
queryVptree <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, 
    raw.index=FALSE, warn.ties=TRUE, ...)
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=query_vptree, searchArgsFUN=.find_vptree_args, ...)
}

#' @export
#' @rdname queryKNN-functions
#' @importFrom BiocParallel SerialParam bpmapply
queryExhaustive <- function(X, query, k, get.index=TRUE, get.distance=TRUE, last=k, 
    BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, 
    raw.index=FALSE, warn.ties=TRUE, ...)
{
    .template_query_knn(X, query, k, get.index=get.index, get.distance=get.distance, 
        last=last, BPPARAM=BPPARAM, precomputed=precomputed, transposed=transposed, subset=subset, 
        exact=TRUE, warn.ties=warn.ties, raw.index=raw.index, 
        buildFUN=buildExhaustive, searchFUN=query_exhaustive, searchArgsFUN=.find_exhaustive_args, ...)
}
