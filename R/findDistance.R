#' Distance to the k-th nearest neighbor
#' 
#' Find the distance to the k-th nearest neighbor for each point in a dataset.
#' 
#' @inheritParams findKNN
#' @param ... For \code{findDistanceFromIndex}, further arguments to pass to individual methods.
#' If a method accepts arguments here, it should prefix these arguments with the algorithm name to avoid conflicts, e.g., \code{vptree.foo.bar}.
#' 
#' For \code{findDistance}, further arguments to pass to \code{findDistanceFromIndex}.
#' These are also passed to \code{\link{buildIndex}} when \code{X} is not an external pointer.
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{findDistance} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' Numeric vector of length equal to the number of points in \code{X} (or \code{subset}, if provided),
#' containing the distance from each point to its \code{k}-th nearest neighbor.
#' This is equivalent to but more memory efficient than using \code{\link{findKNN}} and subsetting to the last distance.
#'
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex}}, to build an index ahead of time.
#'
#' @aliases
#' findDistanceFromIndex,BiocNeighborGenericIndex-method
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- findDistance(Y, k=8)
#' summary(out)
#' 
#' @name findDistance
NULL

#' @export
#' @rdname findDistance
setMethod("findDistanceFromIndex", "BiocNeighborGenericIndex", function(BNINDEX, k, num.threads=1, subset=NULL, ...) {
    generic_find_knn(
        BNINDEX@ptr, 
        num_neighbors=as.integer(k), 
        force_variable_neighbors=is(k, "AsIs"),
        chosen=.integerize_subset(BNINDEX, subset), 
        num_threads=num.threads, 
        last_distance_only=TRUE,
        report_index=FALSE,
        report_distance=FALSE
    )
})

#' @export
#' @rdname findDistance
findDistance <- function(X, k, num.threads=1, subset=NULL, ..., BNPARAM=NULL) {
    if (!is(X, "BiocNeighborIndex")) {
        X <- buildIndex(X, ..., BNPARAM=BNPARAM)
    }

    findDistanceFromIndex(
        X,
        k=k,
        num.threads=num.threads,
        subset=subset,
        ...
    )
}
