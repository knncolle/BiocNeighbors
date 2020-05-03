#' Pre-cluster points with k-means
#'
#' Perform k-means clustering in preparation for a KMKNN nearest-neighbors search.
#'
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param distance String specifying the type of distance to use.
#' @param ... Further arguments to pass to \code{\link{kmeans}}.
#'
#' @details
#' This function is automatically called by \code{\link{findKmknn}} and related functions.
#' However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.
#'
#' @return
#' A \linkS4class{KmknnIndex} object containing:
#' \itemize{
#' \item \code{data}, a numeric matrix with points in the \emph{columns} and dimensions in the rows, i.e., transposed relative to the input.
#' Points have also been reordered to improve data locality during the nearest-neighbor search.
#' Specifically, points in the same cluster are contiguous and ordered by increasing distance from the cluster center.
#' \item \code{clusters}, itself a list containing:
#' \itemize{
#'   \item \code{centers}, a numeric matrix of cluster center coordinates where each column corresponds to a cluster.
#'   \item \code{info}, another list of length equal to the number of clusters.
#'   Each entry corresponds to a column of \code{centers} (let's say cluster \eqn{j}) and is a list of length 2.
#'   The first element is the zero-index of the first cell in the \emph{output} \code{X} that is assigned to \eqn{j}.
#'   The second element is the distance of each point in the cluster from the cluster center.
#' }
#' \item \code{order}, an integer vector specifying how rows in \code{X} have been reordered in columns of \code{data}.
#' \item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
#' \item \code{distance}, a string specifying the distance metric used.
#' }
#'
#' @seealso
#' See \code{\link{kmeans}} for optional arguments.
#' See \code{\link{KmknnIndex}} for details on the output class.
#' See \code{\link{findKmknn}}, \code{\link{queryKmknn}} and \code{\link{findNeighbors}} for dependent functions.
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- buildKmknn(Y)
#' out
#' 
#' @author Aaron Lun
#' @export
#' @importFrom stats kmeans
#' @importFrom methods is
#' @importFrom Matrix t
buildKmknn <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan"), ...) 
# Reorganizing the matrix 'x' for fast lookup via K-means clustering.
#
# written by Aaron Lun
# created 19 June 2018
{
    if (transposed) {
        X <- t(X)
    }
    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }
    distance <- match.arg(distance)

    N <- ceiling(sqrt(nrow(X)))
    if (N==nrow(X)) {
        # Every point is its own cluster.
        out <- list(cluster=seq_len(N), centers=X)
    } else if (ncol(X)==0L) {
        # Every point is in the same cluster.           
        out <- list(cluster=rep(1L, nrow(X)), centers=matrix(0, 1, 0))
    } else { 
        out <- tryCatch(suppressWarnings(kmeans(X, centers=N, ...)), error=identity)
        if (is(out, "error")) { 
            out <- suppressWarnings(kmeans(jitter(X), centers=N, ...))
        }
    }
    
    by.clust <- split(seq_len(nrow(X)), out$cluster)
    accumulated <- 0L
    nclust <- length(by.clust) # should be N, but redefining just in case...
    clust.info <- new.X <- ordering <- vector("list", nclust)

    # Adding stubs to ensure we get objects out of the intended type.
    new.X[[1]] <- t(X[0,,drop=FALSE])
    ordering[[1]] <- integer(0)

    # Compiling to something that can be quickly accessed at the C++ level.
    for (clust in seq_len(nclust)) {
        chosen <- by.clust[[clust]]
        current.vals <- t(X[chosen,,drop=FALSE])

        diff <- out$centers[clust,] - current.vals
        cur.dist <- switch(distance,
            Euclidean=sqrt(colSums(diff^2)),
            Manhattan=colSums(abs(diff))
        )

        o <- order(cur.dist)
        new.X[[clust]] <- current.vals[,o,drop=FALSE]
        ordering[[clust]] <- chosen[o]

        cur.dist <- cur.dist[o]
        clust.info[[clust]] <- list(accumulated, cur.dist)
        accumulated <- accumulated + length(o)
    }
   
    KmknnIndex(data=do.call(cbind, new.X), centers=t(out$centers), info=clust.info, order=unlist(ordering), NAMES=rownames(X), distance=distance)
} 
