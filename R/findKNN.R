#' @export
#' @importFrom BiocParallel SerialParam bplapply
findKNN <- function(X, k, get.index=TRUE, get.distance=TRUE, query=NULL, BPPARAM=SerialParam(), precomputed=NULL, query.transposed=FALSE) 
# Identifies nearest neighbours 
#
# written by Aaron Lun
# created 19 June 2018
{
    if (is.null(start)) {
        start <- precluster(X)
    }

    if (!is.null(query)) { 
        if (!query.transposed) {
            query <- t(query)
        }
        query <- as.matrix(query)
        njobs <- ncol(query)
    } else {
        njobs <- ncol(start$X)
    }

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(njobs, BPPARAM)
    collected <- bplapply(jobs, FUN=.find_nn, X=start$X, centers=start$clusters$centers, 
        info=start$clusters$info, k=k, query=query, get.index=get.index, get.distance=get.distance)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        output$index <- do.call(rbind, lapply(collected, "[[", i=1))
    } 
    if (get.distance) {
        output$distance <- do.call(rbind, lapply(collected, "[[", i=2))
    }
    return(output)
}

.find_knn <- function(current, X, centers, info, k, query, get.index, get.distance) {
    .Call(cxx_find_knn, X, centers, info, k, get.index, get.distance)
}

