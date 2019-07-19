#' @importFrom BiocParallel SerialParam bpmapply
.template_range_find_exact <- function(X, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, 
    buildFUN, searchFUN, searchArgsFUN, ...)
# Template to identify neighbours within 'threshold' distance.
#
# written by Aaron Lun
# created 20 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index, buildFUN=buildFUN, ...)
    ind.out <- .setup_indices(precomputed, subset, raw.index)
    job.id <- ind.out$index
    reorder <- ind.out$reorder

    # Allow for variable thresholds across data points.
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
    if (bpnworkers(BPPARAM)==1L) {
        jobs <- list(job.id-1L)
        thresh <- list(thresholds)
    } else {
        jobs <- .assign_jobs(job.id, BPPARAM)
        thresh <- vector("list", length(jobs))
        for (i in seq_along(jobs)) {
            thresh[[i]] <- thresholds[jobs[[i]]]
            jobs[[i]] <- jobs[[i]] - 1L
        }
    }

    collected <- bpmapply(FUN=searchFUN, 
        to_check=jobs, dist_thresh=thresh,
        MoreArgs=c(
            searchArgsFUN(precomputed), 
            list(X=bndata(precomputed), dtype=bndistance(precomputed), get_index=get.index, get_distance=get.distance)
        ), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_lists(collected, i=1, reorder=reorder)
        if (!raw.index) {
            preorder <- bnorder(precomputed)
            neighbors <- lapply(neighbors, FUN=function(i) preorder[i])
        }
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_lists(collected, i=2, reorder=reorder)
    }
    return(output)
}
