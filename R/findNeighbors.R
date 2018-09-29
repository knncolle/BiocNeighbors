#' @export
#' @importFrom BiocParallel SerialParam bpmapply
findNeighbors <- function(X, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, ...)
# Identifies neighbours within 'threshold' distance.
#
# written by Aaron Lun
# created 20 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index, ...)
    ind.out <- .setup_indices(precomputed, subset, raw.index)
    job.id <- ind.out$index
    reorder <- ind.out$reorder

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(FUN=.find_neighbors, jobs,
        MoreArgs=list(data=KmknnIndex_clustered_data(precomputed),
            centers=KmknnIndex_cluster_centers(precomputed),
            info=KmknnIndex_cluster_info(precomputed),
            threshold=threshold,
            get.index=get.index, 
            get.distance=get.distance), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_lists(collected, i=1, reorder=reorder)
        if (!raw.index) {
            preorder <- KmknnIndex_clustered_order(precomputed)
            neighbors <- lapply(neighbors, FUN=function(i) preorder[i])
        }
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_lists(collected, i=2, reorder=reorder)
    }
    return(output)
}

.find_neighbors <- function(jobs, data, centers, info, threshold, get.index, get.distance) {
    .Call(cxx_find_neighbors, jobs, data, centers, info, threshold, get.index, get.distance)
}
