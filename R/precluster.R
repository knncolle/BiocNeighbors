#' @export
#' @importFrom stats kmeans
precluster <- function(X, ...) 
# Reorganizing the matrix 'x' for fast lookup via K-means clustering.
#
# written by Aaron Lun
# created 19 June 2018
{
    N <- ceiling(sqrt(nrow(X)))
    if (N==nrow(X)) {
        out <- list(cluster=seq_len(N), centers=X)
    } else if (ncol(X)==0L) {
        out <- list(cluster=rep(1L, nrow(X)), centers=matrix(0, 1, 0))
    } else { 
        tryCatch({
            out <- suppressWarnings(kmeans(X, centers=N, ...))
        }, error=function(e) {
        }, finally={
            # Protecting by adding jitter, if many observations are duplicated.
            out <- suppressWarnings(kmeans(jitter(X), centers=N, ...))
        })
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
        cur.dist <- sqrt(colSums((out$centers[clust,] - current.vals)^2))

        o <- order(cur.dist)
        new.X[[clust]] <- current.vals[,o,drop=FALSE]
        ordering[[clust]] <- chosen[o]

        cur.dist <- cur.dist[o]
        clust.info[[clust]] <- list(accumulated, cur.dist)
        accumulated <- accumulated + length(o)
    }
   
    return(list(X=do.call(cbind, new.X), 
                clusters=list(centers=t(out$centers), info=clust.info),
                order=unlist(ordering)))
} 


