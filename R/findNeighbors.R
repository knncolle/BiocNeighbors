#' @export
#' @importFrom BiocParallel SerialParam bpmapply
findNeighbors <- function(X, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL)
# Identifies neighbours within 'threshold' distance.
#
# written by Aaron Lun
# created 20 June 2018
{
    if (is.null(precomputed)) {
        precomputed <- precluster(X)
    }

    # Defining indices of interest, accounting for re-ordering.
    if (!is.null(subset)) { 
        indices <- .subset_to_index(subset, X, byrow=TRUE)

        # Getting position in reordered 'precomputed$X'.
        new.pos <- integer(length(precomputed$order))
        new.pos[precomputed$order] <- seq_along(new.pos)
        job.id <- new.pos[indices] 

        # Ordering so that queries are as adjacent as possible.
        reorder <- order(job.id)
        job.id <- job.id[reorder]
    } else {
        job.id <- seq_len(nrow(X))
        reorder <- precomputed$order
    }

    # Dividing jobs up for NN finding (using bpmapply due to clash with 'X').
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(FUN=.find_neighbors, jobs,
        MoreArgs=list(X=precomputed$X, 
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
        neighbors <- lapply(neighbors, FUN=function(i) precomputed$order[i])
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
