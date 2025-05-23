#' The KmknnParam class
#'
#' A class to hold parameters for the k-means k-nearest-neighbors (KMKNN) algorithm for exact nearest neighbor identification.
#'
#' @inheritParams ExhaustiveParam
#' @param BNPARAM A KmknnParam instance.
#' @param ... Further arguments, ignored.
#'
#' @details
#' In the KMKNN algorithm (Wang, 2012), k-means clustering is first applied to the data points using the square root of the number of points as the number of cluster centers.
#' The cluster assignment and distance to the assigned cluster center for each point represent the KMKNN indexing information. 
#' This speeds up the nearest neighbor search by exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.
#' The advantage of the KMKNN approach is its simplicity and minimal overhead,
#' resulting in performance improvements over conventional tree-based methods for high-dimensional data where most points need to be searched anyway.
#' It is also trivially extended to find all neighbors within a threshold distance from a query point.
#'
#' Note that KMKNN operates much more naturally with Euclidean distances.
#' Computational efficiency may not be optimal when using it with other choices of \code{distance}, though the results will still be exact.
#'
#' @return
#' The \code{KmknnParam} constructor returns an instance of the KmknnParam class.
#'
#' The \code{\link{defineBuilder}} method returns a list that can be used in \code{\link{buildIndex}} to construct a KMKNN index.
#' 
#' @author
#' Aaron Lun, using code from the \pkg{cydar} package. 
#' 
#' @seealso
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @references
#' Wang X (2012). 
#' A fast exact k-nearest neighbors algorithm for high dimensional search using k-means clustering and triangle inequality. 
#' \emph{Proc Int Jt Conf Neural Netw}, 43, 6:2351-2358.
#'
#' @examples
#' (out <- KmknnParam(iter.max=100))
#'
#' @docType class
#' @aliases
#' KmknnParam-class
#' KmknnIndex
#' KmknnIndex-class
#'
#' @export
#' @importFrom methods new
KmknnParam <- function(..., distance=c("Euclidean", "Manhattan", "Cosine")) {
    new("KmknnParam", distance=match.arg(distance))
}

#' @export
KmknnIndex <- function(ptr, names) {
    new("KmknnIndex", ptr=ptr, names=names)
}

#' @export
#' @rdname KmknnParam
setMethod("defineBuilder", "KmknnParam", function(BNPARAM) {
    list(
        builder=kmknn_builder(distance=BNPARAM@distance),
        class=KmknnIndex
    )
})
