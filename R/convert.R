#' Converts \code{findKNN} output to alternative representations 
#' 
#' Convert the output of \code{\link{findKNN}} to alternative representations for more convenient downstream use.
#' 
#' @param knn Named list containing the output from \code{\link{findKNN}}.
#' This list should have the \code{index} and \code{distance} matrices.
#' @param repr String specifing the type of sparse matrix, see \code{?\link[Matrix]{sparseMatrix}} for more details.
#' 
#' @return
#' For \code{convertToSparseMatrix}, a floating-point sparse matrix from the \pkg{Matrix} package.
#' Each row corresponds to an observation and the columns with non-zero entries represent the neighbors.
#' The value of each non-zero entry contains the distance to that neighbor.
#'
#' For \code{convertToSelfHits}, a \link[S4Vectors]{SelfHits} object where \code{from} contains the observations and \code{to} contains the identities of the neighbors.
#' The \code{distance} metadata field contains the distance from each observation to each of its neighbors.
#' 
#' @examples
#' X <- matrix(rnorm(10000), ncol=20)
#' out <- findKNN(X, k=5)
#' 
#' sm <- convertToSparseMatrix(out)
#' str(sm)
#' 
#' sh <- convertToSelfHits(out)
#' sh
#'
#' @seealso
#' The \code{colPairs} slot of the SingleCellExperiment class, from the \pkg{SingleCellExperiment} package.
#' 
#' @export
#' @name convert
convertToSparseMatrix <- function(knn, repr = "C") {
    stopifnot(identical(names(knn), c("index", "distance")))
    n <- nrow(knn$index)
    Matrix::sparseMatrix(
        i = rep(seq_len(n), length.out = length(knn$index)), 
        j = as.vector(knn$index), 
        x = as.vector(knn$distance), 
        dims = c(n, n),
        repr = repr 
    )
}

#' @export
#' @rdname convert
convertToSelfHits <- function(knn){
    stopifnot(identical(names(knn), c("index", "distance")))
    n <- nrow(knn$index)
    S4Vectors::SelfHits(
        from = rep(seq_len(n), length.out = length(knn$index)),
        to = as.vector(knn$index),
        distance = as.vector(knn$distance),
        nnode = n
    )
}
