#' The ExhaustiveParam class
#'
#' A class to hold parameters for the exhaustive algorithm for exact nearest neighbor identification.
#' 
#' @param distance String specifying the distance metric to use.
#' Cosine distances are implemented as Euclidean distances on L2-normalized coordinates.
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param ... Further arguments, ignored.
#' @param BNPARAM An ExhaustiveParam instance.
#' 
#' @details
#' The exhaustive search computes all pairwise distances between data and query points to identify nearest neighbors of the latter.
#' It has quadratic complexity and is theoretically the worst-performing method;
#' however, it has effectively no overhead from constructing or querying indexing structures, 
#' making it faster for in situations where indexing provides little benefit.
#' This includes queries against datasets with few data points or very high dimensionality.
#'
#' All that said, this algorithm is largely provided as a baseline for comparing against the other algorithms.
#'
#' @return
#' The \code{ExhaustiveParam} constructor returns an instance of the ExhaustiveParam class.
#'
#' The \code{\link{buildIndex}} method returns an external pointer to an exhaustive index.
#' 
#' @author
#' Allison Vuong
#' 
#' @seealso
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- ExhaustiveParam())
#'
#' @aliases ExhaustiveParam-class
#' @docType class
#' 
#' @export
#' @importFrom methods new
ExhaustiveParam <- function(distance=c("Euclidean", "Manhattan", "Cosine")) {
    new("ExhaustiveParam", distance=match.arg(distance))
}

#' @export
#' @rdname ExhaustiveParam
setMethod("buildIndex", c("ANY", "ExhaustiveParam"), function(X, transposed = FALSE, ..., BNPARAM) {
    X <- .coerce_matrix_build(X, transposed)
    build_exhaustive(X, distance=BNPARAM@distance)
})
