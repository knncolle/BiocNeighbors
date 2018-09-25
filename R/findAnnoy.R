#' @export
#' @importFrom BiocParallel SerialParam
findAnnoy <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, ntrees=50)
# Find nearest neighbors using the Annoy approximate nearest neighbors algorithm.
# 
# written by Aaron Lun
# created 25 September 2018
{
    if (is.null(precomputed)) {
        precomputed <- buildAnnoy(X, ntrees=ntrees)
        on.exit(unlink(precomputed))
    }

    if (is.null(subset)) {
        job.id <- seq_len(nrow(X))
    } else {
        job.id <- .subset_to_index(subset, X, byrow=TRUE)
    }

    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(FUN=.find_annoy, jobs,
        MoreArgs=list(ndims=ncol(X), 
            k=k, 
            fname=precomputed, 
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

