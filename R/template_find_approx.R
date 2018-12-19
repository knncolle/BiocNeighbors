#' @importFrom BiocParallel SerialParam bpmapply
.template_find_approx <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, 
    buildFUN, pathFUN, searchFUN, searchArgsFUN, ...)
# Provides a R template for approximate nearest neighbors searching, 
# assuming that all of them use a file-backed index.
# 
# written by Aaron Lun
# created 14 December 2018
{
    if (is.null(precomputed)) {
        precomputed <- buildFUN(X, ...)
        on.exit(unlink(pathFUN(precomputed)))
    }

    if (is.null(subset)) {
        job.id <- seq_len(nrow(precomputed))
    } else {
        job.id <- .subset_to_index(subset, precomputed, byrow=TRUE)
    }
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)

    k <- .refine_k(k, precomputed, query=FALSE)
    collected <- bpmapply(FUN=searchFUN, jobs,
        MoreArgs=c(
            searchArgsFUN(precomputed), 
            list(k=k, get.index=get.index, get.distance=get.distance, distance=bndistance(precomputed))
        ), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=NULL)
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=NULL)
    }
    return(output)
}
