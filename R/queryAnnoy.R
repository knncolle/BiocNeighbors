#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryAnnoy <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, ntrees=50)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
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

	# Protection against silliness when k is greater than the number of observations.
    if (k > ncol(X)) { 
        k <- ncol(X) 
        warning("'k' capped at the number of observations")
    }

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(jobs, FUN=.query_annoy,
        MoreArgs=list(ndims=ncol(X),
			fname=precomputed,
            k=k,
            query=query,
            get.index=get.index, 
            get.distance=get.distance), 
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

.query_annoy <- function(jobs, query, ndims, fname, k, get.index, get.distance) {
    .Call(cxx_query_annoy, jobs, query, ndims, fname, k, get.index, get.distance)
}
