#' The HnswParam class
#'
#' A class to hold parameters for the HNSW algorithm for approximate nearest neighbor identification.
#' 
#' @param nlinks Integer scalar, number of bi-directional links per element for index generation.
#' @param ef.construction Integer scalar, size of the dynamic list for index generation.
#' @param directory String specifying the directory in which to save the index.
#' @param ef.search Integer scalar, size of the dynamic list for neighbor searching.
#' @param distance A string specifying the distance metric to use.
#' @inheritParams ExhaustiveParam
#' @param BNPARAM A HsnwParam instance.
#' 
#' @details
#' In the HNSW algorithm (Malkov and Yashunin, 2016), each point is a node in a \dQuote{nagivable small world} graph.
#' The nearest neighbor search proceeds by starting at a node and walking through the graph to obtain closer neighbors to a given query point.
#' Nagivable small world graphs are used to maintain connectivity across the data set by creating links between distant points.
#' This speeds up the search by ensuring that the algorithm does not need to take many small steps to move from one cluster to another.
#' The HNSW algorithm extends this idea by using a hierarchy of such graphs containing links of different lengths, 
#' which avoids wasting time on small steps in the early stages of the search where the current node position is far from the query.
#'
#' Larger values of \code{nlinks} improve accuracy at the expense of speed and memory usage.
#' Larger values of \code{ef.construction} improve index quality at the expense of indexing time.
#' The value of \code{ef.search} controls the accuracy of the neighbor search at run time,
#' where larger values improve accuracy at the expense of a slower search.
#' 
#' Technically, the index construction algorithm is stochastic but, for various logistical reasons, the seed is hard-coded into the C++ code.
#' This means that the results of the HNSW neighbor searches will be fully deterministic for the same inputs, even though the theory provides no such guarantees.
#' 
#' @return
#' The \code{HnswParam} constructor returns an instance of the HnswParam class.
#'
#' The \code{\link{buildIndex}} method returns an external pointer to a HNSW index.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#'
#' \url{https://github.com/nmslib/hnswlib}, for details on the underlying algorithm.
#' 
#' @examples
#' (out <- HnswParam())
#' out[['nlinks']]
#'
#' out[['nlinks']] <- 20L
#' out
#'
#' @aliases
#' HnswParam-class
#' show,HnswParam-method
#' @docType class
#'
#' @export
#' @importFrom methods new
HnswParam <- function(nlinks=16, ef.construction=200, ef.search=10, distance="Euclidean") {
    new("HnswParam", nlinks=as.integer(nlinks), ef.construction=as.integer(ef.construction), ef.search=as.integer(ef.search), distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("HnswParam", function(object) {
    msg <- character(0)

    nlinks <- object[['nlinks']]
    if (length(nlinks) != 1L || nlinks <= 0L) {
        msg <- c(msg, "'nlinks' should be a positive integer scalar")
    }

    ef.construction <- object[['ef.construction']]
    if (length(ef.construction) != 1L || ef.construction <= 0L) {
        msg <- c(msg, "'ef.construction' should be a positive integer scalar")
    }

    ef.search <- object[['ef.search']]
    if (length(ef.search) != 1L || ef.search <= 0L) {
        msg <- c(msg, "'ef.search' should be a positive integer scalar")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
setMethod("show", "HnswParam", function(object) {
    callNextMethod()
    cat(sprintf("nlinks: %i\n", object[['nlinks']]))
    cat(sprintf("ef.construction: %i\n", object[['ef.construction']]))
    cat(sprintf("ef.search: %i\n", object[['ef.search']]))
})

#' @export
#' @rdname HnswParam
setMethod("buildIndex", "HnswParam", function(X, transposed = FALSE, ..., BNPARAM) {
    X <- .coerce_matrix_build(X, transposed)
    build_hnsw(X, nlinks=BNPARAM@nlinks, ef_construct=BNPARAM@ef.construction, ef_search=BNPARAM@ef.search, distance=BNPARAM@distance)
})
