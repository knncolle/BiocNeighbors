#' Find nearest neighbors
#' 
#' Find the nearest neighbors of each point in a dataset, using a variety of algorithms.
#' 
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' Alternatively, a pointer to a prebuilt index from \code{\link{buildIndex}}.
#' @param k A positive integer scalar specifying the number of nearest neighbors to retrieve.
#' @param get.index A logical scalar indicating whether the indices of the nearest neighbors should be recorded.
#' Setting this to \code{FALSE} improves efficiency if the indices are not of interest.
#' @param get.distance A logical scalar indicating whether distances to the nearest neighbors should be recorded.
#' Setting this to \code{FALSE} improves efficiency if the distances are not of interest.
#' @param num.threads Integer scalar specifying the number of threads to use for the search.
#' @param BPPARAM Deprecated, use \code{num.threads} instead.
#' @param subset A vector indicating the rows of \code{X} for which the nearest neighbors should be identified.
#' This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findKmknn} with \code{subset=NULL}.
#' @param ... Further arguments to pass to \code{\link{buildIndex}} when \code{X} is not an external pointer.
#' This includes \code{BNPARAM} to specify how the index should be constructed.
#' 
#' @details
#' If multiple queries are to be performed to the same \code{X}, it may be beneficial to build the index from \code{X} with \code{\link{buildIndex}}.
#' The resulting pointer object can be supplied as \code{X} to multiple \code{findKNN} calls, avoiding the need to repeat index construction in each call.
#' 
#' @return
#' A list is returned containing:
#' \itemize{
#'     \item \code{index}, if \code{get.index=TRUE}.
#'     This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{X}.
#'     The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
#'     \item \code{distance}, if \code{get.distance=TRUE}.
#'     This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
#' }
#'
#' The number of columns in both matrices is set to \code{min(k, ncol(X) - 1)}.
#' If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildIndex}}, to build an index ahead of time.
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- findKNN(Y, k=8)
#' head(out$index)
#' head(out$distance)
#' 
#' @export
findKNN <- function(X, k, get.index=TRUE, get.distance=TRUE, num.threads=1, subset=NULL, ..., BPPARAM=NULL) {
    if (!is(X, "externalptr")) {
        X <- buildIndex(X, ..., BNPARAM)
    }

    if (!is.null(BPPARAM)) {
        num.threads <- BiocParallel::bpnworkers(BPPARAM)
    }

    if (is.null(subset)) {
        output <- generic_find_knn(X, k=k, num_threads=num.threads, report_index=get.index, report_distance=get.distance)
    } else {
        output <- generic_find_knn_subset(X, k=k, chosen=subset, num_threads=num.threads, report_index=get.index, report_distance=get.distance)
    }

    if (!report.index) {
        output$index <- NULL
    }
    if (!report.distance) {
        output$distance <- NULL
    }

    output
}
