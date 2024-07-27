#' Find mutual nearest neighbors
#'
#' Find mutual nearest neighbors (MNN) across two data sets.
#'
#' @param data1 A numeric matrix containing points in the rows and variables/dimensions in the columns.
#' @param data2 A numeric matrix like \code{data1} for another dataset with the same variables/dimensions.
#' @param k1 Integer scalar specifying the number of neighbors to search for in \code{data1}.
#' @param k2 Integer scalar specifying the number of neighbors to search for in \code{data2}.
#' @param BNINDEX1 A pre-built index for \code{data1}.
#' If \code{NULL}, this is constructed from \code{data1} within the internal \code{\link{queryKNN}} call.
#' @param BNINDEX2 A pre-built index for \code{data2}.
#' If \code{NULL}, this is constructed from \code{data2} within the internal \code{\link{queryKNN}} call.
#' @param ... Other arguments to be passed to the underlying \code{\link{queryKNN}} calls, e.g., \code{BNPARAM}, .
#'
#' @return
#' A list containing the integer vectors \code{first} and \code{second}, containing row indices from \code{data1} and \code{data2} respectively.
#' Corresponding entries in \code{first} and \code{second} specify a MNN pair consisting of the specified rows from each matrix.
#'
#' @details
#' For each point in dataset 1, the set of \code{k2} nearest points in dataset 2 is identified.
#' For each point in dataset 2, the set of \code{k1} nearest points in dataset 1 is similarly identified.
#' Two points in different datasets are considered to be part of an MNN pair if each point lies in the other's set of neighbors.
#' This concept allows us to identify matching points across datasets, which is useful for, e.g., batch correction.
#'
#' Any values for the \code{BNINDEX1} and \code{BNINDEX2} arguments should be equal to the output of \code{\link{buildIndex}} for the respective matrices,
#' using the algorithm specified with \code{BNPARAM}.
#' These arguments are only provided to improve efficiency during repeated searches on the same datasets (e.g., for comparisons between all pairs).
#' The specification of these arguments should not, generally speaking, alter the output of the function.
#'
#' @author
#' Aaron Lun
#'
#' @seealso
#' \code{\link{queryKNN}} for the underlying neighbor search code.
#'
#' \code{fastMNN} and related functions from the \pkg{batchelor} package, from which this code was originally derived.
#'
#' @examples
#' B1 <- matrix(rnorm(10000), ncol=50) # Batch 1 
#' B2 <- matrix(rnorm(10000), ncol=50) # Batch 2
#' out <- findMutualNN(B1, B2, k1=20)
#' head(out$first)
#' head(out$second)
#'
#' @export
findMutualNN <- function(data1, data2, k1, k2=k1, BNINDEX1=NULL, BNINDEX2=NULL, ...) {
    data1 <- as.matrix(data1)
    data2 <- as.matrix(data2)

    common.args <- list(..., get.distance=FALSE)

    args <- c(common.args, list(query=data1, k=k2))
    if (!is.null(BNINDEX2)) {
        args$X <- BNINDEX2
    } else {
        args$X <- data2
    }
    W21 <- do.call(queryKNN, args)

    args <- c(common.args, list(query=data2, k=k1))
    if (!is.null(BNINDEX1)) {
        args$X <- BNINDEX1
    } else {
        args$X <- data1
    }
    W12 <- do.call(queryKNN, args)

    out <- find_mutual_nns(W21$index, W12$index)
    names(out) <- c("first", "second")
    out
}
