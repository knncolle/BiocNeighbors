#' @export
#' @importFrom BiocParallel SerialParam 
rangeFindVptree <- function(X, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE, ...) {
    .template_range_find_exact(X, threshold, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, raw.index=raw.index,
        buildFUN=buildVptree, searchFUN=.range_find_vptree, searchArgsFUN=.find_vptree_args, ...) 
}

.range_find_vptree <- function(jobs, data, nodes, distance, threshold, get.index, get.distance) {
    .Call(cxx_range_find_vptree, jobs, data, nodes, distance, threshold, get.index, get.distance)
}

