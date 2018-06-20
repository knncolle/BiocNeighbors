#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryKNN <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL)
# Identifies nearest neighbours.
#
# written by Aaron Lun
# created 19 June 2018
{
    if (is.null(precomputed)) {
        precomputed <- precluster(X)
    }

    # Protection against silliness when k is greater than (or for self-neighbors, equal to) the number of observations.
    if (k > ncol(precomputed$X)) { 
        k <- ncol(precomputed$X) 
        warning("'k' capped at the number of observations")
    }

    # Examining the nature of the query.
    if (!transposed) {
        query <- t(query)
    }
    query <- as.matrix(query)
    nobs <- ncol(query)

    # Choosing indices.
    if (!is.null(subset)) {
        job.id <- .subset_to_index(subset, query, byrow=FALSE)
        reorder <- order(job.id) # ordering so that queries are adjacent.
        job.id <- job.id[o]
    } else {
        job.id <- seq_len(ncol(X))
        reorder <- NULL
    }

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(jobs, FUN=.query_knn,
        MoreArgs=list(X=precomputed$X, 
            centers=precomputed$clusters$centers, 
            info=precomputed$clusters$info, 
            k=k,
            query=query,
            get.index=get.index, 
            get.distance=get.distance), 
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

.query_knn <- function(jobs, X, centers, info, k, query, get.index, get.distance) {
    .Call(cxx_query_knn, jobs, X, centers, info, k, query, get.index, get.distance)
}
