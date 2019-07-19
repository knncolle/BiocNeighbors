#' @export
#' @importFrom BiocParallel SerialParam 
findAnnoy <- function(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, subset=NULL, ...)
# Identifies nearest neighbours with the Kmknn algorithm.
#
# written by Aaron Lun
# created 25 September June 2018
{
    .template_find_approx(X, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, subset=subset, 
        buildFUN=buildAnnoy, pathFUN=AnnoyIndex_path, searchFUN=find_annoy, searchArgsFUN=.find_annoy_args, ...) 
}

.find_annoy_args <- function(precomputed) {
    list(
        ndims=ncol(precomputed),
        fname=AnnoyIndex_path(precomputed),
        search.mult=AnnoyIndex_search_mult(precomputed)
    )
}
