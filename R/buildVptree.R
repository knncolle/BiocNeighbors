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
#' Points in \code{X} are reordered to improve data locality during the nearest-neighbor search.
#' Specifically, points in the same cluster are contiguous and ordered by increasing distance from the cluster center.
#' 
#' The function also reports a list containing four vectors of equal length describing the structure of the VP tree.
#' Each parallel element specifies a node:
#' \itemize{
#'     \item The first integer vector specifies the column index of \code{data} of the current node.
#'     \item The second integer vector specifies the column index of the left child of the current node, 
#'     \item The third integer vector specifies the column index of the right child of the current node.
#'     \item The fourth numeric vector specifies the radius of the current node.
#' }
#' All indices here are zero-based, with child values set to -1 for leaf nodes.
#'
#' @return
#' A \linkS4class{VptreeIndex} object containing indexing structures for the VP-tree search.
#' 
#' @seealso
#' \linkS4class{VptreeIndex}, for details on the output class.
#' 
#' \code{\link{findVptree}} and \code{\link{queryVptree}}, for dependent functions.
#' 
#' @author
#' Aaron Lun
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- buildVptree(Y)
#' out
#' 
#' @export
#' @importFrom Matrix t
buildVptree <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan", "Cosine")) {
    X <- .coerce_matrix_build(X, transposed)
    distance <- match.arg(distance)
    if (distance=="Cosine") {
        X <- l2norm(X)
    }

    out <- build_vptree(X, distance)
    ordering <- out[[1]]
    VptreeIndex(data=X[,ordering,drop=FALSE], order=ordering, nodes=out[-1], NAMES=colnames(X), distance=distance)
}
