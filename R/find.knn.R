#' @export
#' @importFrom BiocParallel SerialParam bpmapply
find.knn <- function(X, k, get.index=TRUE, get.distance=TRUE, query=NULL, BPPARAM=SerialParam(), precomputed=NULL, query.transposed=FALSE) 
# Identifies nearest neighbours.
#
# written by Aaron Lun
# created 19 June 2018
{
    if (is.null(precomputed)) {
        precomputed <- precluster(X)
    }

    self <- is.null(query) 
    if (!self) {
        if (!query.transposed) {
            query <- t(query)
        }
        query <- as.matrix(query)
        nobs <- ncol(query)
        reorder <- NULL
    } else {
        nobs <- ncol(precomputed$X)
        reorder <- precomputed$order
    }

    # Protection against silliness when k is greater than (or for self-neighbors, equal to) the number of observations.
    if (k > ncol(precomputed$X)) { 
        k <- ncol(precomputed$X) 
        if (self) {
            k <- k-1L
        }
        warning("'k' capped at the number of observations")
    }

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(nobs, BPPARAM)
    collected <- bpmapply(FUN=.find_knn, start=jobs$start - 1L, end=jobs$end,
        MoreArgs=list(X=precomputed$X, centers=precomputed$clusters$centers, info=precomputed$clusters$info, k=k, query=query, get.index=get.index, get.distance=get.distance),
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=reorder)
        neighbors[] <- precomputed$order[neighbors]
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=reorder)
    }
    return(output)
}

.find_knn <- function(start, end, X, centers, info, k, query, get.index, get.distance) {
    .Call(cxx_find_knn, start, end, X, centers, info, k, query, get.index, get.distance)
}

.combine_matrices <- function(collected, i, reorder=NULL) {
    all.mat <- lapply(collected, "[[", i=i)
    out <- do.call(cbind, all.mat)
    if (!is.null(reorder)) { 
        out[,reorder] <- out
    }
    t(out)
}
