#' Query for the distance to the k-th nearest neighbor
#'
#' Query a dataset to determine the distance to the k-th nearest neighbor of each point in another dataset, using a variety of algorithms.
#' 
#' @inheritParams queryKNN
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{queryKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' Numeric vector of length equal to the number of points in \code{query} (or \code{subset}, if provided),
#' containing the distance from each point to its \code{k}-th nearest neighbor.
#' This is equivalent to but faster than taking the last distance of the output of \code{\link{queryKNN}}.
#'
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex}}, to build an index ahead of time.
#' 
#' @aliases
#' queryDistance,matrix,ANY-method
#' queryDistance,externalptr,ANY-method
#' queryDistance,matrix-method
#' queryDistance,externalptr-method
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(20000), ncol=20)
#' out <- queryDistance(Y, query=Z, k=5)
#' head(out)
#' 
#' @name queryDistance
NULL

#' @export
setMethod("queryDistance", c("matrix", "ANY"), function(X, query, k, num.threads=1, subset=NULL, transposed=FALSE, ..., BNPARAM=NULL) {
    ptr <- buildIndex(X, transposed=transposed, ..., BNPARAM=BNPARAM)
    callGeneric(ptr, query=query, k=k, get.index=get.index, get.distance=get.distance, num.threads=num.threads, subset=subset, transposed=transposed, ...)
})

#' @export
setMethod("queryDistance", c("externalptr", "ANY"), function(X, query, k, num.threads=1, subset=NULL, transposed=FALSE, ..., BNPARAM=NULL) {
    query <- .coerce_matrix_build(query, transposed)
    if (!is.null(subset)) {
        query <- query[,subset,drop=FALSE] # could move into C++ for efficiency but can't be bothered for now.
    }

    generic_query_knn(
        X,
        query=query,
        num_neighbors=k,
        num_threads=num.threads,
        last_distance_only=TRUE,
        report_index=FALSE,
        report_distance=FALSE
    )
})
