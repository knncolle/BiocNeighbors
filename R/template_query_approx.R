#' @importFrom BiocParallel SerialParam bpmapply
.template_query_approx <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL,
    buildFUN, pathFUN, searchFUN, searchArgsFUN, ...)
# Provides a R template for approximate nearest neighbors querying, 
# assuming that all of them use a file-backed index.
#
# written by Aaron Lun
# created 14 December 2018
{
    if (is.null(precomputed)) {
        precomputed <- buildFUN(X, ...)
        on.exit(unlink(pathFUN(precomputed)))
    }

    k <- .refine_k(k, precomputed, query=TRUE)

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(jobs, FUN=searchFUN,
        MoreArgs=c(searchArgsFUN(precomputed), list(k=k, query=query, get.index=get.index, get.distance=get.distance)), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=reorder)
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=reorder)
    }
    return(output)
}

