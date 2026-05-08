#' Converts a `findKNN` list to a alternative representations
#' 
#' @description These functions are for conveniently inserting the output of 
#' `findKNN` into the `colPair` slot of a `SingleCellExperiment`.
#' 
#' @param knn A named list. The output from `findKNN`, a list of length 2 holding
#' two matrices, `index` and `distance`.
#' 
#' @returns For `convertToSparseMatrix`, a `sparseMatrix` and for `convertToSelfHits`, 
#' a `SelfHits` object. 
#' 
#' #' @examples
#' X <- matrix(rnorm(10000), ncol=20)
#' out <- findKNN(X, k=5)
#' 
#' sm <- convertToSparseMatrix(out)
#' class(sm)
#' 
#' sh <- convertToSelfHits(out)
#' class(sh)
#' 
#' @rdname converters
#' 
#' @export
convertToSparseMatrix <- function(knn){
    # expect named list of length 2
    stopifnot(identical(names(knn), c("index", "distance")))
    stopifnot(is.list(knn) && length(knn) == 2)
    nr <- nrow(knn$index)
    sm <- Matrix::sparseMatrix(
        i = rep(seq_len(nr), length.out = length(knn$index)), 
        j = as.vector(knn$index), 
        x = as.vector(knn$distance), 
        dims = c(nr, nr)
    )
    return(sm)
}

#' @export
#' @rdname converters
convertToSelfHits <- function(knn){
    # expect named list of length 2
    stopifnot(identical(names(knn), c("index", "distance")))
    stopifnot(is.list(knn) && length(knn) == 2)
    nr <- nrow(knn$index)
    sh <- S4Vectors::SelfHits(
        from = rep(seq_len(nr), length.out = length(knn$index)),
        to = as.vector(knn$index),
        x = as.vector(knn$distance),
        nnode = nr
    )
    return(sh)
}

