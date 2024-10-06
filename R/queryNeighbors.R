#' Query neighbors within a threshold distance
#' 
#' Find all points in a reference dataset that lie within a threshold distance of each point in a query dataset.
#' 
#' @inheritParams queryKNN
#' @param threshold A positive numeric scalar specifying the maximum distance at which a point is considered a neighbor.
#' Alternatively, a vector containing a different distance threshold for each query point.
#' @param get.index A logical scalar indicating whether the indices of the neighbors should be recorded.
#' @param get.distance A logical scalar indicating whether distances to the neighbors should be recorded.
#' 
#' @details
#' This function identifies all points in \code{X} that within \code{threshold} of each point in \code{query}.
#' For Euclidean distances, this is equivalent to identifying all points in a hypersphere centered around the point of interest.
#' Not all implementations support this search mode, but we can use \link{KmknnParam} and \linkS4class{VptreeParam}.
#' 
#' If \code{threshold} is a vector, each entry is assumed to specify a (possibly different) threshold for each point in \code{query}.
#' If \code{subset} is also specified, each entry is assumed to specify a threshold for each point in \code{subset}.
#' An error will be raised if \code{threshold} is a vector of incorrect length.
#'
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{queryKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' A list is returned containing:
#' \itemize{
#' \item \code{index}, if \code{get.index=TRUE}.
#' This is a list of integer vectors where each entry corresponds to a point (denoted here as \eqn{i}) in \code{query}.
#' The vector for \eqn{i} contains the set of row indices of all points in \code{X} that lie within \code{threshold} of point \eqn{i}.
#' Neighbors for \eqn{i} are sorted by increasing distance from \eqn{i}.
#' \item \code{distance}, if \code{get.distance=TRUE}.
#' This is a list of numeric vectors where each entry corresponds to a point (as above) and contains the distances of the neighbors from \eqn{i}.
#' Elements of each vector in \code{distance} match to elements of the corresponding vector in \code{index}.
#' }
#'
#' If both \code{get.index=FALSE} and \code{get.distance=FALSE}, an integer vector is returned of length equal to the number of observations.
#' The \code{i}-th entry contains the number of neighbors of \eqn{i} within \code{threshold}.
#' 
#' If \code{subset} is not \code{NULL}, each entry of the above vector/lists refers to a point in the subset, in the same order as supplied in \code{subset}.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex}}, to build an index ahead of time.
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(20000), ncol=20)
#' out <- queryNeighbors(Y, query=Z, threshold=3)
#' summary(lengths(out$index))
#' 
#' @aliases
#' queryNeighbors,matrix,ANY-method
#' queryNeighbors,BiocNeighborIndex,ANY-method
#' queryNeighbors,missing,ANY-method
#' queryNeighbors,matrix-method
#' queryNeighbors,BiocNeighborIndex-method
#' queryNeighbors,missing-method
#'
#' @name queryNeighbors
NULL

#' @export
setMethod("queryNeighbors", c("matrix", "ANY"), function(X, query, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BPPARAM=NULL, BNPARAM=NULL) {
    ptr <- buildIndex(X, transposed=transposed, ..., BNPARAM=BNPARAM)
    callGeneric(ptr, query=query, threshold=threshold, get.index=get.index, get.distance=get.distance, num.threads=num.threads, subset=subset, transposed=transposed, ..., BPPARAM=BPPARAM)
})

#' @export
setMethod("queryNeighbors", c("BiocNeighborIndex", "ANY"), function(X, query, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BPPARAM=NULL, BNPARAM=NULL) {
    if (!is.null(BPPARAM)) {
        num.threads <- BiocParallel::bpnworkers(BPPARAM)
    }

    query <- .coerce_matrix_build(query, transposed)
    if (!is.null(subset)) {
        query <- query[,subset,drop=FALSE] # could move into C++ to avoid a copy but can't be bothered right now.
    }

    output <- generic_query_all(X@ptr, query=query, thresholds=threshold, num_threads=num.threads, report_index=get.index, report_distance=get.distance)

    if (!get.index && !get.distance) {
        return(output)
    } else {
        if (!get.index) {
            output$index <- NULL
        }
        if (!get.distance) {
            output$distance <- NULL
        }
        return(output)
    }
})

#' @export
setMethod("queryNeighbors", c("missing", "ANY"), function(X, query, threshold, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BNINDEX=NULL, BNPARAM=NULL) {
    callGeneric(BNINDEX, query=query, threshold=threshold, get.index=get.index, get.distance=get.distance, num.threads=num.threads, subset=subset, transposed=transposed, ..., BNPARAM=BNPARAM)
})
