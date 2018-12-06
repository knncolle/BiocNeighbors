#' @export
#' @importFrom BiocParallel SerialParam bpmapply
queryVptree <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 2 December 2018
{
    .template_query_exact(X, query, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, 
        transposed=transposed, subset=subset, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=.query_vptree, searchArgsFUN=.find_vptree_args, ...)
}

.query_vptree <- function(jobs, data, nodes, k, query, get.index, get.distance) {
    .Call(cxx_query_vptree, jobs, data, nodes, k, query, get.index, get.distance)
}
