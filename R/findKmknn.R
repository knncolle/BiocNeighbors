#' @export
#' @importFrom BiocParallel SerialParam bpmapply
findKmknn <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE)
# Identifies nearest neighbours.
#
# written by Aaron Lun
# created 19 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index)
    k <- .refine_k(k, precomputed, query=FALSE)

    ind.out <- .setup_indices(precomputed, subset, raw.index)
    job.id <- ind.out$index
    reorder <- ind.out$reorder

    # Dividing jobs up for NN finding (using bpmapply due to clash with 'X=').
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(FUN=.find_knn, jobs,
        MoreArgs=list(data=Kmknn_clustered_data(precomputed),
            centers=Kmknn_cluster_centers(precomputed),
            info=Kmknn_cluster_info(precomputed),
            k=k,
            get.index=get.index, 
            get.distance=get.distance), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=reorder)
        if (!raw.index) {
            neighbors[] <- Kmknn_clustered_order(precomputed)[neighbors]
        }
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=reorder)
    }
    return(output)
}

.find_knn <- function(jobs, data, centers, info, k, get.index, get.distance) {
    .Call(cxx_find_knn, jobs, data, centers, info, k, get.index, get.distance)
}

.setup_precluster <- function(X, precomputed, raw.index) 
# Converts 'X' into 'precomputed' if the latter is NULL.
# This quarantines 'X' from the rest of the function.
{
    if (is.null(precomputed)) {
        if (raw.index) {
            stop("'raw.index=TRUE' is not valid if 'precomputed=NULL'")
        }
        precomputed <- buildKmknn(X)
    }
    precomputed
}

.refine_k <- function(k, precomputed, query=FALSE)
# Protection against silliness when k is greater than or equal to the number of observations (for self-searching),
# or simply greater than the number of observation (for querying).
{
    if (!query) {
        max <- nrow(precomputed) - 1L
        msg <- " minus 1"
    } else {
        max <- nrow(precomputed)
        msg <- ""
    }

    if (k > max) { 
        k <- max
        warning(paste0("'k' capped at the number of observations", msg))
    }

    k
}

.setup_indices <- function(precomputed, subset, raw.index)
# Defining indices of interest, accounting for re-ordering.
{
    if (!is.null(subset)) { 
        indices <- .subset_to_index(subset, precomputed, byrow=TRUE)

        # Getting position in reordered 'precomputed$data'.
        new.pos <- .order_to_index(Kmknn_clustered_order(precomputed))
        job.id <- new.pos[indices] 

        # Ordering so that queries are as adjacent as possible.
        reorder <- order(job.id)
        job.id <- job.id[reorder]
    } else {
        job.id <- seq_len(nrow(precomputed))
        if (raw.index) {
            reorder <- NULL
        } else {
            reorder <- Kmknn_clustered_order(precomputed)
        }
    }

    list(index=job.id, reorder=reorder)
}
