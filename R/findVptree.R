#' @export
#' @importFrom BiocParallel SerialParam 
findVptree <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, raw.index=FALSE)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_find_exact(X, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, raw.index=raw.index, 
        buildFUN=buildVptree, searchFUN=.find_vptree, searchArgsFUN=.find_vptree_args, orderFUN=VptreeIndex_order) 
}

.find_vptree <- function(jobs, data, ordering, nodes, k, get.index, get.distance) {
    .Call(cxx_find_vptree, jobs, data, ordering, nodes, k, get.index, get.distance)
}

.find_vptree_args <- function(precomputed) {
    list(data=VptreeIndex_data(precomputed),
        ordering=VptreeIndex_order(precomputed),
        nodes=VptreeIndex_nodes(precomputed))
}
