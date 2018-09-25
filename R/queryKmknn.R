#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryKmknn <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    pre.out <- .setup_precluster(X, precomputed, raw.index)
    precomputed <- pre.out$precomputed
    X <- pre.out$X

    # Protection against silliness when k is greater than the number of observations.
    if (k > ncol(precomputed$data)) { 
        k <- ncol(precomputed$data) 
        warning("'k' capped at the number of observations")
    }

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(jobs, FUN=.query_knn,
        MoreArgs=list(X=precomputed$data, 
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
        if (!raw.index) {
            neighbors[] <- precomputed$order[neighbors]
        }
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

.setup_query <- function(query, transposed, subset) 
# Convenience wrapper to set up the query.
{
    if (!transposed) {
        query <- t(query)
    }
    if (!is.matrix(query)) {
        query <- as.matrix(query)
    }

    # Choosing indices.
    if (!is.null(subset)) {
        job.id <- .subset_to_index(subset, query, byrow=FALSE)
        reorder <- order(job.id) # ordering so that queries are adjacent.
        job.id <- job.id[reorder]
    } else {
        job.id <- seq_len(ncol(query))
        reorder <- NULL
    }
    return(list(query=query, index=job.id, reorder=reorder))
}
