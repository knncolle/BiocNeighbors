#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryNeighbors <- function(X, query, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 22 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index, buildFUN=buildKmknn, ...)

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    # Allow for variable thresholds across query points. 
    if (length(threshold)==1) {
        thresholds <- rep(threshold, length.out=length(job.id))
    } else if (length(threshold)!=length(job.id)) {
        stop("length of 'threshold' should be equal to number of points specified in 'subset'")
    } else {
        thresholds <- threshold
        if (!is.null(reorder)) {
            thresholds <- thresholds[reorder]
        }
    }

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    thresholds <- .assign_jobs(thresholds, BPPARAM)

    collected <- bpmapply(FUN=.query_neighbors,
        jobs=jobs, threshold=thresholds,
        MoreArgs=list(data=KmknnIndex_clustered_data(precomputed),
            centers=KmknnIndex_cluster_centers(precomputed),
            info=KmknnIndex_cluster_info(precomputed),
            query=query,
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

.query_neighbors <- function(jobs, data, centers, info, threshold, query, get.index, get.distance) {
    .Call(cxx_query_neighbors, jobs, data, centers, info, threshold, query, get.index, get.distance)
}
