#' @importFrom BiocParallel SerialParam bpmapply
.template_range_query_exact <- function(X, query, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, exact=TRUE,
    buildFUN, searchFUN, searchArgsFUN, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 22 June 2018
{
    precomputed <- .setup_precluster(X, precomputed, raw.index, buildFUN=buildFUN, ...)

    q.out <- .setup_query(query, transposed, subset)
    query <- q.out$query        
    job.id <- q.out$index
    reorder <- q.out$reorder

    thresholds <- .define_thresholds(threshold, length(job.id), reorder)

    # Dividing jobs up for NN finding.
    Q <- .split_matrix_for_workers(query, job.id, BPPARAM)
    if (length(Q)==1L){ 
        thresh <- list(thresholds)
    } else {
        thresh <- vector("list", length(Q))
        last <- 0L
        for (i in seq_along(Q)) {
            N <- ncol(Q[[i]])
            ind <- seq_len(N) + last
            thresh[[i]] <- thresholds[ind]
            last <- last + N
        }
    }

    collected <- bpmapply(FUN=searchFUN, query=Q, dist_thresh=thresh,
        MoreArgs=c(
            searchArgsFUN(precomputed), 
            list(X=bndata(precomputed), dtype=bndistance(precomputed), get_index=get.index, get_distance=get.distance)
        ), 
        BPPARAM=BPPARAM, SIMPLIFY=FALSE)

    .form_range_output(collected, precomputed=precomputed, reorder=reorder,
        get.index=get.index, get.distance=get.distance, raw.index=raw.index)
}
