#' Query k-nearest neighbors
#'
#' Query a reference dataset for the k-nearest neighbors of each point in a query dataset.
#' 
#' @param BNINDEX A \linkS4class{BiocNeighborIndex} object, typically created by \code{\link{buildIndex}}.
#' @param X The reference dataset to be queried.
#' This should be a numeric matrix or matrix-like object where rows correspond to reference points and columns correspond to variables (i.e., dimensions).
#' Alternatively, a prebuilt \linkS4class{BiocNeighborIndex} object from \code{\link{buildIndex}}.
#' @inheritParams findKNN
#' @param k A positive integer scalar specifying the number of nearest neighbors to retrieve.
#'
#' Alternatively, an integer vector of length equal to the number of points in \code{query}, specifying the number of neighbors to identify for each point.
#' If \code{subset} is provided, this should have length equal to the length of \code{subset}.
#' Users should wrap this vector in an \link{AsIs} class to distinguish length-1 vectors from integer scalars.
#'
#' All \code{k} should be less than or equal to the number of points in \code{X}, otherwise the former will be capped at the latter with a warning.
#' @param query A numeric matrix or matrix-like object of query points, containing the same number of columns as \code{X}.
#' @param transposed A logical scalar indicating whether \code{query} is transposed, in which case it contains dimensions in the rows and data points in the columns.
#' For \code{queryKNN}, settting \code{transposed=TRUE} also indicates that \code{X} is also transposed.
#' @param subset An integer, logical or character vector indicating the rows of \code{query} (or columns, if \code{transposed=TRUE}) for which the nearest neighbors should be identified.
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{queryKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' List containing \code{index} (if \code{get.index} is not \code{FALSE}) and \code{distance} (if \code{get.distance} is not \code{FALSE}).
#' \itemize{
#' \item 
#' If \code{get.index=TRUE} or \code{"normal"} and \code{k} is an integer scalar,
#' \code{index} is an integer matrix with \code{k} columns where each row corresponds to a point (denoted here as \eqn{i}) in \code{query}.
#' The \eqn{i}-th row contains the indices of points in \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
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
#' \code{\link{queryDistance}}, to obtain the distance from each query point to its k-th nearest neighbor.
#' 
#' @aliases
#' queryKNN,matrix,ANY-method
#' queryKNN,BiocNeighborGenericIndex,ANY-method
#' queryKNN,missing,ANY-method
#' queryKNN,matrix-method
#' queryKNN,BiocNeighborGenericIndex-method
#' queryKNN,missing-method
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(20000), ncol=20)
#' out <- queryKNN(Y, query=Z, k=5)
#' head(out$index)
#' head(out$distance)
#' 
#' @name queryKNN
NULL

#' @export
#' @rdname queryKNN
setMethod("queryKnnFromIndex", "BiocNeighborGenericIndex", function(BNINDEX, query, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ...) {
    query <- .transpose_and_subset(query, transposed, subset=subset)
    if (!is.matrix(query)) {
        query <- beachmat::initializeCpp(query)
    }

    output <- generic_query_knn(
        BNINDEX@ptr,
        query=query,
        num_neighbors=as.integer(k),
        force_variable_neighbors=is(k, "AsIs"),
        num_threads=num.threads,
        last_distance_only=FALSE,
        report_index=!isFALSE(get.index),
        report_distance=!isFALSE(get.distance)
    )

    if (length(k) == 1L) {
        output <- .format_output(output, "index", get.index)
        output <- .format_output(output, "distance", get.distance)
    }

    output
})

#' @export
#' @rdname queryKNN
queryKNN <- function(X, query, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BPPARAM=NULL, BNPARAM=NULL) {
    if (!is.null(BPPARAM)) {
        num.threads <- BiocParallel::bpnworkers(BPPARAM)
    }
    if (!is(X, "BiocNeighborIndex")) {
        X <- buildIndex(X, transposed=transposed, ..., BNPARAM=BNPARAM)
    }

    queryKnnFromIndex(
        X,
        query=query,
        k=k,
        get.index=get.index,
        get.distance=get.distance,
        num.threads=num.threads,
        subset=subset,
        transposed=transposed,
        ...
    )
}
