#' Query nearest neighbors
#'
#' Query a dataset for nearest neighbors of points in another dataset, using a variety of algorithms.
#' 
#' @inheritParams findKNN
#' @param query A numeric matrix of query points, containing the same number of columns as \code{X}.
#' @param transposed A logical scalar indicating whether \code{X} and \code{query} are transposed, 
#' in which case both matrices are assumed to contain dimensions in the rows and data points in the columns.
#' @param subset A vector indicating the rows of \code{query} for which the nearest neighbors should be identified.
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{queryKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' A list is returned containing:
#' \itemize{
#' \item \code{index}, if \code{get.index=TRUE}.
#' This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{query}.
#' The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
#' \item \code{distance}, if \code{get.distance=TRUE}.
#' This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
#' }
#'
#' The number of columns in both matrices is set to \code{min(k, ncol(X))}.
#' If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.
#' 
#' If \code{get.index="transposed"}, the \code{index} matrix is transposed, i.e., the rows are the neighbors while the columns are the points.
#' Similarly, if \code{get.distance="transposed"}, the \code{distance} matrix is transposed.
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
#' out <- queryKNN(Y, query=Z, k=5)
#' head(out$index)
#' head(out$distance)
#' 
#' @export
queryKNN <- function(X, query, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, transposed=FALSE, ..., BPPARAM=NULL) {
    if (!is(X, "externalptr")) {
        X <- buildIndex(X, transposed=transposed, ...)
    }

    if (!is.null(BPPARAM)) {
        num.threads <- BiocParallel::bpnworkers(BPPARAM)
    }

    query <- .coerce_matrix_build(query, transposed)
    if (!is.null(subset)) {
        query <- query[,subset,drop=FALSE]
    }

    output <- generic_query_knn(X, query=query, k=k, num_threads=num.threads, report_index=get.index, report_distance=get.distance)

    output <- .format_output(output, "index", get.index)
    output <- .format_output(output, "distance", get.distance)

    output
}
