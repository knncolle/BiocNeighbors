#' Prepare data for an Exhaustive search. 
#'
#' Transform data in prepartaion for an Exhaustive search. 
#'
#' @param X A numeric matrix where rows correspond to data points 
#' and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, 
#' i.e., rows are variables and columns are data points.
#' @param distance String specifying the type of distance to use.
#' 
#' @return A \linkS4class{KmknnIndex} object containing:
#' \itemize{
#' \item \code{data}, a numeric matrix with points in the \emph{columns} and dimensions in the rows, 
#' i.e., transposed relative to the input.
#' \item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
#' \item \code{distance}, a string specifying the distance metric used.
#' } 
#' 
#' @importFrom Matrix t
#' @export
buildExhaustive <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan"))
# Builds an Exhaustive index.
# 
# created 19 April 2020
{
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
