#' @export
#' @importFrom BiocParallel SerialParam bpmapply
rangeQueryKmknn <- function(X, query, threshold, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), precomputed=NULL, transposed=FALSE, subset=NULL, raw.index=FALSE, ...)
# Identifies nearest neighbours in 'X' from a query set.
#
# written by Aaron Lun
# created 19 June 2018
{
    .template_range_query_exact(X, query, threshold, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, precomputed=precomputed, 
        transposed=transposed, subset=subset, raw.index=raw.index, 
        buildFUN=buildKmknn, searchFUN=.range_query_kmknn, searchArgsFUN=.find_kmknn_args, ...) 
}

.range_query_kmknn <- function(jobs, data, centers, info, threshold, query, get.index, get.distance) {
    .Call(cxx_range_query_kmknn, jobs, data, centers, info, threshold, query, get.index, get.distance)
}
