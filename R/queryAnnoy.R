#' @export
#' @importFrom BiocParallel SerialParam 
queryAnnoy <- function(X, query, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_query_approx(X, query, k, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed,
        transposed=transposed, subset=subset, 
        buildFUN=buildAnnoy, pathFUN=AnnoyIndex_path, searchFUN=query_annoy, searchArgsFUN=.find_annoy_args, ...)
}
