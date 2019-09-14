#' @importFrom BiocParallel SerialParam bpmapply
.template_find_knn <- function(X, k, get.index=TRUE, get.distance=TRUE, 
    last=k, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, 
    exact=TRUE, warn.ties=TRUE, raw.index=FALSE, 
    buildFUN, pathFUN, searchFUN, searchArgsFUN, ...) 
# Provides an R template for different methods for exact neighbor searching,
# assuming that all of them involve rearranging columns in the index.
#
# written by Aaron Lun
# created 2 December 2018
{
    if (exact) {
        precomputed <- .setup_precluster(X, precomputed, raw.index, buildFUN=buildFUN, ...)

        ind.out <- .setup_indices(precomputed, subset, raw.index)
        job.id <- ind.out$index
        reorder <- ind.out$reorder

        common.args <- list(X=bndata(precomputed), warn_ties=warn.ties)
    } else {
        if (is.null(precomputed)) {
            precomputed <- buildFUN(X, ...)
            on.exit(unlink(pathFUN(precomputed)))
        }

        if (is.null(subset)) {
            job.id <- seq_len(nrow(precomputed))
        } else {
            job.id <- .subset_to_index(subset, precomputed, byrow=TRUE)
        }

        common.args <- list()
        reorder <- NULL
    }

    k <- .refine_k(k, precomputed, query=FALSE)
    last <- min(last, k)

    # Dividing jobs up for NN finding (using bpmapply due to clash with 'X=').
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    common.args <- c(common.args, searchArgsFUN(precomputed),
        list(dtype=bndistance(precomputed), nn=k, last=last))

    collected <- bpmapply(FUN=searchFUN, to_check=jobs,
        MoreArgs=c(common.args, list(get_index=get.index, get_distance=get.distance)), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=reorder)
        if (exact && !raw.index) {
            neighbors[] <- bnorder(precomputed)[neighbors]
        }
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=reorder)
    }

    output
}

.setup_precluster <- function(X, precomputed, raw.index, buildFUN, ...) 
# Converts 'X' into 'precomputed' if the latter is NULL.
# This quarantines 'X' from the rest of the function.
{
    if (is.null(precomputed)) {
        if (raw.index) {
            stop("'raw.index=TRUE' is not valid if 'precomputed=NULL'")
        }
        precomputed <- buildFUN(X, ...)
    }
    precomputed
}

.refine_k <- function(k, precomputed, query=FALSE)
# Protection against silliness when k is greater than or equal to the number of
# observations (for self-searching), or simply greater than the number of
# observation (for querying).
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
        if (raw.index) {
            # For raw indices, get the actual ordering of names for match()ing.
            dummy <- precomputed
            dummy@NAMES <- dummy@NAMES[bnorder(precomputed)]
            job.id <- .subset_to_index(subset, dummy, byrow=TRUE)
        } else {
            # Getting position of subset indices in the reordered set of points.
            new.pos <- .order_to_index(bnorder(precomputed))
            indices <- .subset_to_index(subset, precomputed, byrow=TRUE)
            job.id <- new.pos[indices]
        }

        # Ordering so that queries are as adjacent as possible.
        reorder <- order(job.id)
        job.id <- job.id[reorder]
    } else {
        job.id <- seq_len(nrow(precomputed))
        if (raw.index) {
            reorder <- NULL
        } else {
            reorder <- bnorder(precomputed)
        }
    }

    list(index=job.id, reorder=reorder)
}
