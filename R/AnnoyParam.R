#' The AnnoyParam class
#'
#' A class to hold parameters for the Annoy algorithm for approximate nearest neighbor identification.
#' 
#' @param ntrees Integer scalar, number of trees to use for index generation.
#' @param search.mult Numeric scalar, multiplier for the number of points to search.
#' @inheritParams ExhaustiveParam
#' @param BNPARAM An AnnoyParam instance.
#' 
#' @details
#' The Approximate nearest neighbors Oh Yeah (Annoy) algorithm is based on recursive hyperplane partitions.
#' Briefly, a tree is constructed where a random hyperplane splits the points into two subsets at each internal node.
#' Leaf nodes are defined when the number of points in a subset falls below a threshold (close to twice the number of dimensions for the settings used here).
#' Multiple trees are constructed in this manner, each of which is different due to the random choice of hyperplanes.
#' For a given query point, each tree is searched to identify the subset of all points in the same leaf node as the query point. 
#' The union of these subsets across all trees is exhaustively searched to identify the actual nearest neighbors to the query.
#'
#' The \code{ntrees} parameter controls the trade-off between accuracy and computational work.
#' More trees provide greater accuracy at the cost of more computational work (both in terms of the indexing time and search speed in downstream functions).
#' 
#' The \code{search.mult} controls the parameter known as \code{search_k} in the original Annoy documentation. 
#' Specifically, \code{search_k} is defined as \code{k * search.mult} where \code{k} is the number of nearest neighbors to identify in downstream functions.
#' This represents the number of points to search exhaustively and determines the run-time balance between speed and accuracy.
#' The default \code{search.mult=ntrees} is based on the Annoy library defaults.
#' Note that this parameter is not actually used in the index construction itself, and is only included here so that the output index fully parametrizes the search.
#' 
#' Technically, the index construction algorithm is stochastic but, for various logistical reasons, the seed is hard-coded into the C++ code.
#' This means that the results of the Annoy neighbor searches will be fully deterministic for the same inputs, even though the theory provides no such guarantees.
#' 
#' @return
#' The \code{AnnoyParam} constructor returns an instance of the AnnoyParam class.
#'
#' The \code{\link{buildIndex}} method returns an external pointer to an Annoy index.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#'
#' \url{https://github.com/spotify/annoy}, for details on the underlying algorithm.
#' 
#' @examples
#' (out <- AnnoyParam())
#' out[['ntrees']]
#'
#' out[['ntrees']] <- 20L
#' out
#'
#' @aliases
#' AnnoyParam-class
#' show,AnnoyParam-method
#'
#' @docType class
#' 
#' @export
#' @importFrom methods new
AnnoyParam <- function(ntrees=50, search.mult=ntrees, distance=c("Euclidean", "Manhattan", "Cosine")) {
    new("AnnoyParam", ntrees=as.integer(ntrees), distance=match.arg(distance), search.mult=search.mult)
}

setValidity("AnnoyParam", function(object) {
    msg <- character(0)

    ntrees <- object[['ntrees']]
    if (length(ntrees) != 1L || ntrees <= 0L) {
        msg <- c(msg, "'ntrees' should be a positive integer scalar")
    }

    search.mult <- object[['search.mult']]
    if (length(search.mult)!=1L || is.na(search.mult) || search.mult <= 1) {
        msg <- c(msg, "'search.mult' should be a numeric scalar greater than 1")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", object[['ntrees']]))
    cat(sprintf("search.mult: %i\n", object[['search.mult']]))
})

#' @export
#' @rdname AnnoyParam
setMethod("buildIndex", c("ANY", "AnnoyParam"), function(X, transposed = FALSE, ..., BNPARAM) {
    X <- .coerce_matrix_build(X, transposed)
    build_annoy(X, num_trees=BNPARAM@ntrees, search_mult=BNPARAM@search.mult, distance=BNPARAM@distance)
})
