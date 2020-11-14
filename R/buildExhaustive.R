#' Prepare data for an exhaustive search 
#'
#' Transform data in preparation for an exhaustive (i.e., brute-force) search. 
#'
#' @param X A numeric matrix where rows correspond to data points 
#' and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, 
#' i.e., rows are variables and columns are data points.
#' @param distance String specifying the type of distance to use.
#' 
#' @details
#' This algorithm is largely provided as a baseline for comparing against the other algorithms.
#' On rare occasions, it may actually be useful in, e.g., very high-dimensional data 
#' where the indexing step of other algorithms adds computational overhead for no benefit.
#' 
#' @return An \linkS4class{ExhaustiveIndex} object containing indexed data.
#' 
#' @author Allison Vuong
#'
#' @seealso
#' \linkS4class{ExhaustiveIndex}, for details on the output class.
#'
#' \code{\link{findExhaustive}} and \code{\link{queryExhaustive}}, for dependent functions.
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- buildExhaustive(Y)
#' out
#'
#' @export
#' @importFrom Matrix t
buildExhaustive <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan")) {
    if (transposed) {
        tX <- X
    } else {
        tX <- t(X)
    }
    if (!is.matrix(tX)) {
        tX <- as.matrix(tX)
    }
    distance <- match.arg(distance)

    ExhaustiveIndex(data=tX, NAMES=colnames(tX), distance=distance)
}
