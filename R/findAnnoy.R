#' @export
#' @importFrom BiocParallel SerialParam
findAnnoy <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, ...)
# Find nearest neighbors using the Annoy approximate nearest neighbors algorithm.
# 
# written by Aaron Lun
# created 25 September 2018
{
    if (is.null(precomputed)) {
        precomputed <- buildAnnoy(X, ...)
        on.exit(unlink(AnnoyIndex_path(precomputed)))
    }

    if (is.null(subset)) {
        job.id <- seq_len(nrow(precomputed))
    } else {
        job.id <- .subset_to_index(subset, precomputed, byrow=TRUE)
    }
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)

    k <- .refine_k(k, precomputed, query=FALSE)
    collected <- bpmapply(FUN=.find_annoy, jobs,
        MoreArgs=list(ndims=ncol(precomputed), 
            k=k, 
            fname=AnnoyIndex_path(precomputed), 
            get.index=get.index, 
            get.distance=get.distance), 
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

.find_annoy <- function(jobs, ndims, fname, k, get.index, get.distance) {
    .Call(cxx_find_annoy, jobs, ndims, fname, k, get.index, get.distance)
}
