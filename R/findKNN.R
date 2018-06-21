#' @export
#' @importFrom BiocParallel SerialParam bpmapply
findKNN <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE)
# Identifies nearest neighbours.
#
# written by Aaron Lun
# created 19 June 2018
{
    pre.out <- .setup_precluster(X, precomputed, raw.index)
    precomputed <- pre.out$precomputed
    X <- pre.out$X

    # Protection against silliness when k is greater than or equal to the number of observations.
    if (k >= ncol(precomputed$data)) { 
        k <- ncol(precomputed$data) - 1L
        warning("'k' capped at the number of observations minus 1")
    }

    ind.out <- .setup_indices(X, precomputed, subset, raw.index)
    job.id <- ind.out$index
    reorder <- ind.out$reorder

    # Dividing jobs up for NN finding (using bpmapply due to clash with 'X=').
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(FUN=.find_knn, jobs,
        MoreArgs=list(X=precomputed$data, 
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
        if (!raw.index) {
            neighbors[] <- precomputed$order[neighbors]
        }
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

#' @importFrom DelayedArray DelayedArray t
.setup_precluster <- function(X, precomputed, raw.index) 
# Checks the input of various arguments related to preclustering.
{
    if (is.null(precomputed)) {
        precomputed <- precluster(X)
        if (raw.index) {
            stop("'raw.index=TRUE' is not valid if 'precomputed=NULL'")
        }
    } else {
        X <- DelayedArray(precomputed$data)
        if (raw.index) {
            precomputed$order <- seq_along(precomputed$order) # Turning off any downstream reordering.
        } else {
            X <- X[,.order_to_index(precomputed$order),drop=FALSE]
        }
        X <- t(X)
    }
    list(precomputed=precomputed, X=X)
}

.setup_indices <- function(X, precomputed, subset, raw.index)
# Defining indices of interest, accounting for re-ordering.
{
    if (!is.null(subset)) { 
        indices <- .subset_to_index(subset, X, byrow=TRUE)

        # Getting position in reordered 'precomputed$data'.
        new.pos <- .order_to_index(precomputed$order)
        job.id <- new.pos[indices] 

        # Ordering so that queries are as adjacent as possible.
        reorder <- order(job.id)
        job.id <- job.id[reorder]
    } else {
        job.id <- seq_len(ncol(precomputed$data))
        if (raw.index) {
            reorder <- NULL
        } else {
            reorder <- precomputed$order
        }
    }

    list(index=job.id, reorder=reorder)
}
