#' Query nearest neighbors
#'
#' Query a dataset for nearest neighbors of points in another dataset, using a variety of algorithms.
#'
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param query A numeric matrix of query points, containing different data points in the rows but the same number and ordering of dimensions in the columns.
#' @param k A positive integer scalar specifying the number of nearest neighbors to retrieve.
#' @param get.index A logical scalar indicating whether the indices of the nearest neighbors should be recorded.
#' @param get.distance A logical scalar indicating whether distances to the nearest neighbors should be recorded.
#' @param last An integer scalar specifying the number of furthest neighbors for which statistics should be returned.
#' @param BPPARAM A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.
#' @param precomputed A \linkS4class{BiocNeighborIndex} object of the appropriate class, generated from \code{X}. 
#' For \code{queryExhaustive}, this should be a \linkS4class{ExhaustiveIndex} from \code{\link{buildExhaustive}};
#' for \code{queryKmknn}, this should be a \linkS4class{KmknnIndex} from \code{\link{buildKmknn}};
#' for \code{queryVptree}, this should be a \linkS4class{VptreeIndex} from \code{\link{buildVptree}};
#' for \code{queryAnnoy}, this should be a \linkS4class{AnnoyIndex} from \code{\link{buildAnnoy}};
#' and for \code{queryHnsw}, this should be a \linkS4class{HnswIndex} from \code{\link{buildHnsw}}. 
#' @param transposed A logical scalar indicating whether the \code{query} is transposed,
#' in which case \code{query} is assumed to contain dimensions in the rows and data points in the columns.
#' @param subset A vector indicating the rows of \code{query} (or columns, if \code{transposed=TRUE}) for which the nearest neighbors should be identified.
#' @param raw.index A logial scalar indicating whether raw column indices should be returned,
#' see \code{?"\link{BiocNeighbors-raw-index}"}.
#' This argument is ignored for \code{queryAnnoy} and \code{queryHnsw}.
#' @param warn.ties Logical scalar indicating whether a warning should be raised if any of the \code{k+1} neighbors have tied distances.
#' This argument is ignored for \code{queryAnnoy} and \code{queryHnsw}.
#' @param ... Further arguments to pass to the respective \code{build*} function for each algorithm.
#' This includes \code{distance}, a string specifying whether \code{"Euclidean"} or \code{"Manhattan"} distances are to be used.
#'
#' @details
#' All of these functions identify points in \code{X} that are the \code{k} nearest neighbors of each point in \code{query}.
#' \code{queryHnsw} and \code{queryAnnoy} perform an approximate search, while \code{queryExhaustive}, \code{queryKmknn} and \code{queryVptree} are exact.
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
#' @return List containing: 
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
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{buildExhaustive}},
#' \code{\link{buildKmknn}},
#' \code{\link{buildVptree}},
#' \code{\link{buildHnsw}},
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
#' @name queryKNN methods
#' @aliases queryKNN-methods
NULL

##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.QUERYKNN_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, query, k, ..., BNINDEX, BNPARAM) {
        do.call(FUN, c(list(X=X, query=query, k=k, ...), ARGS(BNPARAM)))
    }
}

#' @importFrom BiocParallel SerialParam
.QUERYKNN_GENERATOR_NOX <- function(FUN) {
    function(X, query, k, ..., BNINDEX, BNPARAM) {
        FUN(query=query, k=k, ..., precomputed=BNINDEX)
    }
}

####################
# Default dispatch #
####################

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("missing", "missing"), .QUERYKNN_GENERATOR(queryKNN, .default_param))

####################
# Specific methods #
####################

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("missing", "KmknnParam"), .QUERYKNN_GENERATOR(queryKmknn))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("KmknnIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryKmknn))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("KmknnIndex", "KmknnParam"), .QUERYKNN_GENERATOR_NOX(queryKmknn))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("missing", "VptreeParam"), .QUERYKNN_GENERATOR(queryVptree))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("VptreeIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryVptree))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("VptreeIndex", "VptreeParam"), .QUERYKNN_GENERATOR_NOX(queryVptree))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("missing", "AnnoyParam"), .QUERYKNN_GENERATOR(queryAnnoy))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("AnnoyIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryAnnoy))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("AnnoyIndex", "AnnoyParam"), .QUERYKNN_GENERATOR_NOX(queryAnnoy))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("missing", "HnswParam"), .QUERYKNN_GENERATOR(queryHnsw))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("HnswIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryHnsw))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("HnswIndex", "HnswParam"), .QUERYKNN_GENERATOR_NOX(queryHnsw))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("missing", "ExhaustiveParam"), .QUERYKNN_GENERATOR(queryExhaustive))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("ExhaustiveIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryExhaustive))

#' @exportMethod queryKNN
#' @rdname queryKNN
#' @aliases queryKNN,queryKNN-method
setMethod("queryKNN", c("ExhaustiveIndex", "ExhaustiveParam"), .QUERYKNN_GENERATOR_NOX(queryExhaustive))
