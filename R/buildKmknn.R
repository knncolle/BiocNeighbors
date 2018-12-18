#' @export
#' @importFrom stats kmeans
#' @importFrom methods is
#' @importFrom BiocGenerics t
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
   
    KmknnIndex(data=do.call(cbind, new.X), centers=t(out$centers), info=clust.info, order=unlist(ordering), NAMES=rownames(X))
} 
