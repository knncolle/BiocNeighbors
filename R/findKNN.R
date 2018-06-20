#' @export
#' @importFrom BiocParallel SerialParam bpmapply
findKNN <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL)
# Identifies nearest neighbours.
#
# written by Aaron Lun
# created 19 June 2018
{
    if (is.null(precomputed)) {
        precomputed <- precluster(X)
    }

    # Protection against silliness when k >= the number of observations.
    if (k >= ncol(precomputed$X)) { 
        k <- ncol(precomputed$X) - 1L
        warning("'k' capped at the number of observations minus 1")
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
    collected <- bpmapply(FUN=.find_knn, jobs,
        MoreArgs=list(X=precomputed$X, 
            centers=precomputed$clusters$centers, 
            info=precomputed$clusters$info, 
            k=k,
            get.index=get.index, 
            get.distance=get.distance), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=reorder)
        neighbors[] <- precomputed$order[neighbors]
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=reorder)
    }
    return(output)
}

.find_knn <- function(jobs, X, centers, info, k, get.index, get.distance) {
    .Call(cxx_find_knn, jobs, X, centers, info, k, get.index, get.distance)
}

