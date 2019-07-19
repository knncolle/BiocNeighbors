##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.FINDNEIGHBORS_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, threshold, subset=NULL, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) {
        do.call(FUN, 
            c(
                list(X=X, threshold=threshold, subset=subset, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, ...),
                ARGS(BNPARAM)
            )
        )
    }
}

#' @importFrom BiocParallel SerialParam
.FINDNEIGHBORS_GENERATOR_NOX <- function(FUN) {
    function(X, threshold, subset=NULL, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) {
        FUN(threshold=threshold, subset=subset, get.index=get.index, get.distance=get.distance, BPPARAM=BPPARAM, ..., precomputed=BNINDEX)
    }
}

####################
# Default dispatch #
####################

#' @export
setMethod("findNeighbors", c("missing", "missing"), .FINDNEIGHBORS_GENERATOR(findNeighbors, .default_param))

####################
# Specific methods #
####################

#' @export
setMethod("findNeighbors", c("missing", "KmknnParam"), .FINDNEIGHBORS_GENERATOR(rangeFindKmknn))

#' @export
setMethod("findNeighbors", c("KmknnIndex", "missing"), .FINDNEIGHBORS_GENERATOR_NOX(rangeFindKmknn))

#' @export
setMethod("findNeighbors", c("KmknnIndex", "KmknnParam"), .FINDNEIGHBORS_GENERATOR_NOX(rangeFindKmknn))

#' @export
setMethod("findNeighbors", c("missing", "VptreeParam"), .FINDNEIGHBORS_GENERATOR(rangeFindVptree))

#' @export
setMethod("findNeighbors", c("VptreeIndex", "missing"), .FINDNEIGHBORS_GENERATOR_NOX(rangeFindVptree))

#' @export
setMethod("findNeighbors", c("VptreeIndex", "VptreeParam"), .FINDNEIGHBORS_GENERATOR_NOX(rangeFindVptree))
