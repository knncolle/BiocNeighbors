#' @export
#' @importFrom BiocParallel SerialParam bpmapply
find.knn <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL)
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

        # getting position in reordered 'precomputed$X'.
        new.pos <- integer(length(precomputed$order))
        new.pos[precomputed$order] <- seq_along(new.pos)
        job.id <- new.pos[indices] 

        # ordering so that queries are adjacent.
        reorder <- order(job.id)
        job.id <- job.id[o]
    } else {
        job.id <- seq_len(ncol(X))
        reorder <- precomputed$order
    }

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bplapply(jobs, FUN=.find_knn, 
        X=precomputed$X, 
        centers=precomputed$clusters$centers, 
        info=precomputed$clusters$info, 
        k=k,
        get.index=get.index, 
        get.distance=get.distance, 
        BPPARAM=BPPARAM)

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

.find_knn <- function(start, end, X, centers, info, k, query, get.index, get.distance) {
    .Call(cxx_find_knn, start, end, X, centers, info, k, query, get.index, get.distance)
}

.combine_matrices <- function(collected, i, reorder=NULL) {
    all.mat <- lapply(collected, "[[", i=i)
    out <- do.call(cbind, all.mat)
    if (!is.null(reorder)) { 
        out[,reorder] <- out
    }
    t(out)
}
