#' @export
#' @importFrom BiocParallel SerialParam 
queryHnsw <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_approx(X, query, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed,
        transposed=transposed, subset=subset, 
        buildFUN=buildHnsw, pathFUN=HnswIndex_path, searchFUN=.query_hnsw, searchArgsFUN=.find_hnsw_args, ...)
}

.query_hnsw <- function(jobs, query, data, fname, distance, k, get.index, get.distance) {
    .Call(cxx_query_hnsw, jobs, query, data, fname, distance, k, get.index, get.distance)
}
