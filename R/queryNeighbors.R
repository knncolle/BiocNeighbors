#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryNeighbors <- function(X, query, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 22 June 2018
{
    pre.out <- .setup_precluster(X, precomputed, raw.index)
    precomputed <- pre.out$precomputed
    X <- pre.out$X

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(jobs, FUN=.query_neighbors,
        MoreArgs=list(X=precomputed$data, 
            centers=precomputed$clusters$centers, 
            info=precomputed$clusters$info, 
            threshold=threshold,
            query=query,
            get.index=get.index, 
            get.distance=get.distance), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_lists(collected, i=1, reorder=reorder)
        if (!raw.index) {
            neighbors <- lapply(neighbors, FUN=function(i) precomputed$order[i])
        }
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_lists(collected, i=2, reorder=reorder)
    }
    return(output)
}

.query_neighbors <- function(jobs, X, centers, info, threshold, query, get.index, get.distance) {
    .Call(cxx_query_neighbors, jobs, X, centers, info, threshold, query, get.index, get.distance)
}
