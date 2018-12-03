#' @importFrom BiocParallel SerialParam bpmapply
.template_range_query_exact <- function(X, query, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE,
    buildFUN, searchFUN, searchArgsFUN, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 22 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index, buildFUN=buildFUN, ...)

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

    collected <- bpmapply(FUN=searchFUN,
        jobs=jobs, threshold=thresholds,
        MoreArgs=c(searchArgsFUN(precomputed), list(data=bndata(precomputed), query=query, get.index=get.index, get.distance=get.distance)), 
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
