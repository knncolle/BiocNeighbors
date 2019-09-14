##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.FINDKNN_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, k, subset=NULL, get.index=TRUE, get.distance=TRUE, last=k, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) {
        do.call(FUN, 
            c(
                list(X=X, k=k, subset=subset, get.index=get.index, get.distance=get.distance, 
                    last=last, BPPARAM=BPPARAM, ...),
                ARGS(BNPARAM)
            )
        )
    }
}

#' @importFrom BiocParallel SerialParam
.FINDKNN_GENERATOR_NOX <- function(FUN) {
    function(X, k, subset=NULL, get.index=TRUE, get.distance=TRUE, last=k, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) {
        FUN(k=k, subset=subset, get.index=get.index, get.distance=get.distance, 
            last=last, BPPARAM=BPPARAM, ..., precomputed=BNINDEX)
    }
}

####################
# Default dispatch #
####################

#' @export
setMethod("findKNN", c("missing", "missing"), .FINDKNN_GENERATOR(findKNN, .default_param))

####################
# Specific methods #
####################

#' @export
setMethod("findKNN", c("missing", "KmknnParam"), .FINDKNN_GENERATOR(findKmknn))

#' @export
setMethod("findKNN", c("KmknnIndex", "KmknnParam"), .FINDKNN_GENERATOR_NOX(findKmknn))

#' @export
setMethod("findKNN", c("KmknnIndex", "missing"), .FINDKNN_GENERATOR_NOX(findKmknn))

#' @export
setMethod("findKNN", c("missing", "VptreeParam"), .FINDKNN_GENERATOR(findVptree))

#' @export
setMethod("findKNN", c("VptreeIndex", "VptreeParam"), .FINDKNN_GENERATOR_NOX(findVptree))

#' @export
setMethod("findKNN", c("VptreeIndex", "missing"), .FINDKNN_GENERATOR_NOX(findVptree))

#' @export
setMethod("findKNN", c("missing", "AnnoyParam"), .FINDKNN_GENERATOR(findAnnoy))

#' @export
setMethod("findKNN", c("AnnoyIndex", "AnnoyParam"), .FINDKNN_GENERATOR_NOX(findAnnoy))

#' @export
setMethod("findKNN", c("AnnoyIndex", "missing"), .FINDKNN_GENERATOR_NOX(findAnnoy))

#' @export
setMethod("findKNN", c("missing", "HnswParam"), .FINDKNN_GENERATOR(findHnsw))

#' @export
setMethod("findKNN", c("HnswIndex", "HnswParam"), .FINDKNN_GENERATOR_NOX(findHnsw))

#' @export
setMethod("findKNN", c("HnswIndex", "missing"), .FINDKNN_GENERATOR_NOX(findHnsw))
