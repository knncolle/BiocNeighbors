#' Distance to the k-th nearest neighbor to query points
#'
#' Query a reference dataset to determine the distance to the k-th nearest neighbor of each point in a query dataset.
#' 
#' @inheritParams queryKNN
#' @param ... For \code{queryDistanceFromIndex}, further arguments to pass to individual methods.
#' If a method accepts arguments here, it should prefix these arguments with the algorithm name to avoid conflicts, e.g., \code{vptree.foo.bar}.
#'
#' For \code{queryDistance}, further arguments to pass to \code{queryDistanceFromIndex}.
#' These are also passed to \code{\link{buildIndex}} when \code{X} is not an external pointer.
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{queryKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' Numeric vector of length equal to the number of points in \code{query} (or \code{subset}, if provided),
#' containing the distance from each point to its \code{k}-th nearest neighbor.
#' This is equivalent to but more memory efficient than using \code{\link{queryKNN}} and subsetting to the last distance.
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
#' out <- queryDistance(Y, query=Z, k=5)
#' head(out)
#' 
#' @name queryDistance
NULL

#' @export
#' @rdname queryDistance
setMethod("queryDistanceFromIndex", "BiocNeighborGenericIndex", function(
    BNINDEX,
    query,
    k,
    num.threads=1,
    subset=NULL,
    transposed=FALSE,
    ...,
    .check.nonfinite=TRUE
) {
    query <- .transpose_and_subset(query, transposed, subset=subset)
    if (!is.matrix(query)) {
        query <- beachmat::initializeCpp(query)
    }

    generic_query_knn(
        BNINDEX@ptr,
        query=query,
        num_neighbors=as.integer(k), 
        force_variable_neighbors=is(k, "AsIs"),
        num_threads=num.threads,
        last_distance_only=TRUE,
        report_index=FALSE,
        report_distance=FALSE,
        fail_nonfinite=.check.nonfinite
    )
})

#' @export
#' @rdname queryDistance
queryDistance <- function(X, query, k, num.threads=1, ..., subset=NULL, transposed=FALSE, BNPARAM=NULL) {
    if (!is(X, "BiocNeighborIndex")) {
        X <- buildIndex(X, transposed=transposed, ..., BNPARAM=BNPARAM)
    }

    queryDistanceFromIndex(
        X,
        query,
        k=k,
        num.threads=num.threads,
        subset=subset,
        transposed=transposed,
        ...
    )
}
