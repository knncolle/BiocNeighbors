#' @importFrom BiocParallel SerialParam bpmapply
.template_query_exact <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, 
    buildFUN, searchFUN, searchArgsFUN, orderFUN, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index, buildFUN=buildFUN, ...)
    k <- .refine_k(k, precomputed, query=TRUE)

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    # Dividing jobs up for NN finding.
    jobs <- .assign_jobs(job.id - 1L, BPPARAM)
    collected <- bpmapply(jobs, FUN=searchFUN, 
        MoreArgs=c(searchArgsFUN(precomputed), list(k=k, query=query, get.index=get.index, get.distance=get.distance)), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    # Aggregating results across cores.
    output <- list()
    if (get.index) {
        neighbors <- .combine_matrices(collected, i=1, reorder=reorder)
        if (!raw.index) {
            neighbors[] <- orderFUN(precomputed)[neighbors]
        }
        output$index <- neighbors
    } 
    if (get.distance) {
        output$distance <- .combine_matrices(collected, i=2, reorder=reorder)
    }
    return(output)
}

.setup_query <- function(query, transposed, subset) 
# Convenience wrapper to set up the query.
{
    if (!transposed) {
        query <- t(query)
    }
    if (!is.matrix(query)) {
        query <- as.matrix(query)
    }

    # Choosing indices.
    if (!is.null(subset)) {
        job.id <- .subset_to_index(subset, query, byrow=FALSE)
        reorder <- order(job.id) # ordering so that queries are adjacent.
        job.id <- job.id[reorder]
    } else {
        job.id <- seq_len(ncol(query))
        reorder <- NULL
    }
    return(list(query=query, index=job.id, reorder=reorder))
}
