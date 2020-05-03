#' Build a VP tree
#'
#' Build a vantage point tree in preparation for a nearest-neighbors search.
#'
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param distance String specifying the type of distance to use.
#'
#' @details
#' This function is automatically called by \code{\link{findVptree}} and related functions.
#' However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.
#'
#' @return
#' A \linkS4class{VptreeIndex} object containing:
#' \itemize{
#' \item \code{data}, a numeric matrix with points in the \emph{columns} and dimensions in the rows, i.e., transposed relative to the input.
#' Points have also been reordered to improve data locality during the nearest-neighbor search.
#' \item \code{nodes}, a list containing four vectors of equal length describing the structure of the VP tree.
#' Each parallel element specifies a node.
#' \itemize{
#'     \item The first integer vector specifies the column index of \code{data} of the current node.
#'     \item The second integer vector specifies the column index of the left child of the current node,
#'     \item The third integer vector specifies the column index of the right child of the current node.
#'     \item The fourth numeric vector specifies the radius of the current node.
#' }
#' All indices here are zero-based, with child values set to -1 for leaf nodes.
#' \item \code{order}, an integer vector specifying how rows in \code{X} have been reordered in columns of \code{data}.
#' \item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
#' \item \code{distance}, a string specifying the distance metric used.
#' }
#'
#' @seealso
#' See \code{\link{VptreeIndex}} for details on the output class.
#' See \code{\link{findVptree}}, \code{\link{queryKmknn}} and \code{\link{findNeighbors}} for dependent functions.
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- buildVptree(Y)
#' out
#' 
#' @author Aaron Lun
#' @export
#' @importFrom Matrix t
buildVptree <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan"))
# Builds an VP tree index.
# 
# written by Aaron Lun
# created 2 December 2018
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

    out <- build_vptree(tX, distance)
    ordering <- out[[1]]
    VptreeIndex(data=tX[,ordering,drop=FALSE], order=ordering, nodes=out[-1], NAMES=colnames(tX), distance=distance)
}
