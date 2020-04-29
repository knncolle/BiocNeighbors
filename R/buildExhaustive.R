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
#' @return An \linkS4class{ExhaustiveIndex} object containing:
#' \itemize{
#' \item \code{data}, a numeric matrix with points in the \emph{columns} and dimensions in the rows, 
#' i.e., transposed relative to the input.
#' \item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
#' \item \code{distance}, a string specifying the distance metric used.
#' } 
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
