#' Find the distance to the k-th nearest neighbors
#' 
#' Find the distance to the k-th nearest neighbor for each point in a dataset, using a variety of algorithms.
#' 
#' @inheritParams findKNN
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{findDistance} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' Numeric vector of length equal to the number of points in \code{X} (or \code{subset}, if provided),
#' containing the distance from each point to its \code{k}-th nearest neighbor.
#' This is equivalent to but faster than taking the last distance of the output from \code{\link{findKNN}}.
#'
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex}}, to build an index ahead of time.
#'
#' @aliases
#' findDistance,matrix,ANY-method
#' findDistance,externalptr,ANY-method
#' findDistance,matrix-method
#' findDistance,externalptr-method
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- findDistance(Y, k=8)
#' summary(out)
#' 
#' @name findDistance
NULL

#' @export
setMethod("findDistance", c("matrix", "ANY"), function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BNPARAM=NULL) {
    ptr <- buildIndex(X, ..., BNPARAM=BNPARAM)
    callGeneric(ptr, k=k, get.index=get.index, get.distance=get.distance, num.threads=num.threads, subset=subset, ...)
})

#' @export
setMethod("findDistance", c("externalptr", "ANY"), function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BNPARAM=NULL) {
    generic_find_knn(
        X, 
        num_neighbors=k, 
        chosen=subset, 
        num_threads=num.threads, 
        last_distance_only=TRUE,
        report_index=FALSE,
        report_distance=FALSE
    )
})
