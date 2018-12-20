#' @export
#' @importFrom BiocParallel SerialParam
findHnsw <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, ...)
# Find nearest neighbors using the Hnsw approximate nearest neighbors algorithm.
# 
# written by Aaron Lun
# created 14 December 2018
{
    .template_find_approx(X, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        buildFUN=buildHnsw, pathFUN=HnswIndex_path, searchFUN=.find_hnsw, searchArgsFUN=.find_hnsw_args, ...)
}

.find_hnsw <- function(jobs, data, fname, ef.search, distance, k, get.index, get.distance) {
    .Call(cxx_find_hnsw, jobs, data, fname, ef.search, distance, k, get.index, get.distance)
}

.find_hnsw_args <- function(precomputed) {
    list(
        data=bndata(precomputed),
        fname=HnswIndex_path(precomputed),
        ef.search=HnswIndex_ef_search(precomputed)
    )
}
