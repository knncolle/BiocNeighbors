#' Find k-nearest neighbors
#' 
#' Find the k-nearest neighbors of each point in a dataset.
#' 
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' Alternatively, a prebuilt index from \code{\link{buildIndex}}.
#' @param k A positive integer scalar specifying the number of nearest neighbors to retrieve.
#'
#' Alternatively, an integer vector of length equal to the number of points in \code{X}, specifying the number of neighbors to identify for each point.
#' If \code{subset} is provided, this should have length equal to the length of \code{subset}.
#' Users should wrap this vector in an \link{AsIs} class to distinguish length-1 vectors from integer scalars.
#'
#' All \code{k} should be less than or equal to the number of points in \code{X} minus 1, otherwise the former will be capped at the latter with a warning.
#' @param get.index A logical scalar indicating whether the indices of the nearest neighbors should be recorded.
#' Setting this to \code{FALSE} improves efficiency if the indices are not of interest.
#' 
#' Alternatively, if \code{k} is an integer scalar, this may be a string containing \code{"normal"} or \code{"transposed"}.
#' The former is the same as \code{TRUE}, while the latter returns the index matrix in transposed format.
#' @param get.distance A logical scalar indicating whether distances to the nearest neighbors should be recorded.
#' Setting this to \code{FALSE} improves efficiency if the distances are not of interest.
#'
#' Alternatively, if \code{k} is an integer scalar, this may be a string containing \code{"normal"} or \code{"transposed"}.
#' The former is the same as \code{TRUE}, while the latter returns the distance matrix in transposed format.
#' @param num.threads Integer scalar specifying the number of threads to use for the search.
#' @param subset An integer, logical or character vector specifying the indices of points in \code{X} for which the nearest neighbors should be identified.
#' This yields the same result as (but is more efficient than) subsetting the output matrices after computing neighbors for all points. 
#' @param ... Further arguments to pass to \code{\link{buildIndex}} when \code{X} is not an external pointer.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying how the index should be constructed.
#' If \code{NULL}, this defaults to a \linkS4class{KmknnParam}.
#' Ignored if \code{x} contains a prebuilt index.
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{findKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' List containing \code{index} (if \code{get.index} is not \code{FALSE}) and \code{distance} (if \code{get.distance} is not \code{FALSE}).
#' \itemize{
#' \item 
#' If \code{get.index=TRUE} or \code{"normal"} and \code{k} is an integer scalar,
#' \code{index} is an integer matrix with \code{k} columns where each row corresponds to a point (denoted here as \eqn{i}) in \code{X}.
#' The \eqn{i}-th row contains the indices of points in \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
#' \eqn{i} will \emph{not} be included in its own set of nearest neighbors.
#' 
#' If \code{get.index=FALSE} or \code{"transposed"} and \code{k} is an integer scalar,
#' \code{index} is as described above but transposed, i.e., the \code{i}-th column contains the indices of neighboring points in \code{X}. 
#'
#' \item 
#' If \code{get.distance=TRUE} or \code{"normal"} and \code{k} is an integer scalar,
#' \code{distance} is a numeric matrix of the same dimensions as \code{index}.
#' The \eqn{i}-th row contains the distances of neighboring points in \code{X} to the point \eqn{i}, sorted in increasing order.
#'
#' If \code{get.distance=FALSE} or \code{"transposed"} and \code{k} is an integer scalar,
#' \code{distance} is as described above but transposed, i.e., the \code{i}-th column contains the distances to neighboring points in \code{X}. 
#'
#' \item 
#' If \code{get.index} is not \code{FALSE} and \code{k} is an integer vector,
#' \code{index} is a list of integer vectors where each vector corresponds to a point (denoted here as \eqn{i}) in \code{X}.
#' The \eqn{i}-th vector has length \code{k[i]} and contains the indices of points in \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
#'
#' \item 
#' If \code{get.distance} is not \code{FALSE} and \code{k} is an integer vector,
#' \code{distance} is a list of numeric vectors of the same lengths as those in \code{index}.
#' The \eqn{i}-th vector contains the distances of neighboring points in \code{X} to the point \eqn{i}, sorted in increasing order.
#' }
#'
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex}}, to build an index ahead of time.
#'
#' \code{\link{findDistance}}, to efficiently obtain the distance to the k-th nearest neighbor.
#'
#' @aliases
#' findKNN,matrix,ANY-method
#' findKNN,externalptr,ANY-method
#' findKNN,missing,ANY-method
#' findKNN,matrix-method
#' findKNN,externalptr-method
#' findKNN,missing-method
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- findKNN(Y, k=8)
#' head(out$index)
#' head(out$distance)
#' 
#' @name findKNN
NULL

#' @export
setMethod("findKNN", c("matrix", "ANY"), function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BPPARAM=NULL, BNPARAM=NULL) {
    ptr <- buildIndex(X, ..., BNPARAM=BNPARAM)
    callGeneric(ptr, k=k, get.index=get.index, get.distance=get.distance, num.threads=num.threads, subset=subset, ..., BPPARAM=BPPARAM) 
})

#' @export
setMethod("findKNN", c("externalptr", "ANY"), function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BPPARAM=NULL, BNPARAM=NULL) {
    if (!is.null(BPPARAM)) {
        num.threads <- BiocParallel::bpnworkers(BPPARAM)
    }

    output <- generic_find_knn(
        X, 
        num_neighbors=as.integer(k), 
        force_variable_neighbors=is(k, "AsIs"),
        chosen=subset, 
        num_threads=num.threads, 
        last_distance_only=FALSE,
        report_index=!isFALSE(get.index), 
        report_distance=!isFALSE(get.distance)
    )

    output <- .format_output(output, "index", get.index)
    output <- .format_output(output, "distance", get.distance)
    output
})

#' @export
setMethod("findKNN", c("missing", "ANY"), function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BNINDEX=NULL, BNPARAM=NULL) {
    callGeneric(BNINDEX, k=k, get.index=get.index, get.distance=get.distance, num.threads=num.threads, subset=subset, ..., BNPARAM=BNPARAM) 
})
