#' @importFrom BiocParallel SerialParam
.QUERYNEIGHBORS_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, query, threshold, subset=NULL, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) {
        do.call(FUN, 
            c(
                list(X=X, query=query, threshold=threshold, subset=subset, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, ...),
                ARGS(BNPARAM)
            )
        )
    }
}

#' @importFrom BiocParallel SerialParam
.QUERYNEIGHBORS_GENERATOR_NOX <- function(FUN) {
    function(X, query, threshold, subset=NULL, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) {
        FUN(query=query, threshold=threshold, subset=subset, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, ..., precomputed=BNINDEX)
    }
}


####################
# Default dispatch #
####################

#' @export
setMethod("queryNeighbors", c("missing", "missing"), .QUERYNEIGHBORS_GENERATOR(queryNeighbors, .default_param))

####################
# Specific methods #
####################

#' @export
setMethod("queryNeighbors", c("missing", "KmknnParam"), .QUERYNEIGHBORS_GENERATOR(rangeQueryKmknn))

#' @export
setMethod("queryNeighbors", c("KmknnIndex", "missing"), .QUERYNEIGHBORS_GENERATOR_NOX(rangeQueryKmknn))

#' @export
setMethod("queryNeighbors", c("KmknnIndex", "KmknnParam"), .QUERYNEIGHBORS_GENERATOR_NOX(rangeQueryKmknn))

#' @export
setMethod("queryNeighbors", c("missing", "VptreeParam"), .QUERYNEIGHBORS_GENERATOR(rangeQueryVptree))

#' @export
setMethod("queryNeighbors", c("VptreeIndex", "missing"), .QUERYNEIGHBORS_GENERATOR_NOX(rangeQueryVptree))

#' @export
setMethod("queryNeighbors", c("VptreeIndex", "VptreeParam"), .QUERYNEIGHBORS_GENERATOR_NOX(rangeQueryVptree))
