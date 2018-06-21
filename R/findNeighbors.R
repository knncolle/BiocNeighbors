#' @export
#' @importFrom BiocParallel SerialParam bpmapply
findNeighbors <- function(X, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE)
# Identifies neighbours within 'threshold' distance.
#
# written by Aaron Lun
# created 20 June 2018
{
    pre.out <- .setup_precluster(X, precomputed, raw.index)
    precomputed <- pre.out$precomputed
    X <- pre.out$X

    ind.out <- .setup_indices(X, precomputed, subset, raw.index)
    job.id <- ind.out$index
    reorder <- ind.out$reorder

    # Dividing jobs up for NN finding (using bpmapply due to clash with 'X').
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(FUN=.find_neighbors, jobs,
        MoreArgs=list(X=precomputed$data, 
            centers=precomputed$clusters$centers, 
            info=precomputed$clusters$info, 
            threshold=threshold,
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

.find_neighbors <- function(jobs, X, centers, info, threshold, get.index, get.distance) {
    .Call(cxx_find_neighbors, jobs, X, centers, info, threshold, get.index, get.distance)
}
