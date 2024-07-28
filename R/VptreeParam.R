#' The VptreeParam class
#'
#' A class to hold parameters for the vantage point (VP) tree algorithm for exact nearest neighbor identification.
#' 
#' @inheritParams ExhaustiveParam
#' @param BNPARAM A VptreeParam instance.
#' 
#' @details
#' In a VP tree (Yianilos, 1993), each node contains a subset of points that is split into two further partitions.
#' The split is determined by picking an arbitrary point inside that subset as the node center, 
#' computing the distance to all other points from the center, and taking the median as the \dQuote{radius}.
#' The left child of this node contains all points within the median distance from the radius, while the right child contains the remaining points.
#' This is applied recursively until all points resolve to individual nodes.
#' The nearest neighbor search traverses the tree and exploits the triangle inequality between query points, node centers and thresholds to narrow the search space.
#'
#' VP trees are often faster than more conventional KD-trees or ball trees as the former uses the points themselves as the nodes of the tree,
#' avoiding the need to create many intermediate nodes and reducing the total number of distance calculations.
#' Like KMKNN, it is also trivially extended to find all neighbors within a threshold distance from a query point.
#'
#' @return  
#' The \code{VptreeParam} constructor returns an instance of the VptreeParam class.
#'
#' The \code{\link{buildIndex}} method returns an external pointer to an VP tree index.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#'
#' \url{http://stevehanov.ca/blog/index.php?id=130}, for a description of the algorithm.
#'
#' @references 
#' Yianilos PN (1993).
#' Data structures and algorithms for nearest neighbor search in general metric spaces.
#' \emph{Proceedings of the Fourth Annual ACM-SIAM Symposium on Discrete Algorithms}, 311-321.
#' 
#' @examples
#' (out <- VptreeParam())
#'
#' @aliases
#' VptreeParam-class
#'
#' @export
#' @importFrom methods new
VptreeParam <- function(distance=c("Euclidean", "Manhattan", "Cosine")) {
    new("VptreeParam", distance=match.arg(distance))
}

#' @export
#' @rdname VptreeParam
setMethod("buildIndex", c("ANY", "VptreeParam"), function(X, transposed=FALSE, ..., BNPARAM) {
    X <- .coerce_matrix_build(X, transposed)
    build_vptree(X, BNPARAM@distance)
})
