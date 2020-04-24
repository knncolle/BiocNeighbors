##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.QUERYKNN_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, query, k, ..., BNINDEX, BNPARAM) {
        do.call(FUN, c(list(X=X, query=query, k=k, ...), ARGS(BNPARAM)))
    }
}

#' @importFrom BiocParallel SerialParam
.QUERYKNN_GENERATOR_NOX <- function(FUN) {
    function(X, query, k, ..., BNINDEX, BNPARAM) {
        FUN(query=query, k=k, ..., precomputed=BNINDEX)
    }
}

####################
# Default dispatch #
####################

#' @export
setMethod("queryKNN", c("missing", "missing"), .QUERYKNN_GENERATOR(queryKNN, .default_param))

####################
# Specific methods #
####################

#' @export
setMethod("queryKNN", c("missing", "KmknnParam"), .QUERYKNN_GENERATOR(queryKmknn))

#' @export
setMethod("queryKNN", c("KmknnIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryKmknn))

#' @export
setMethod("queryKNN", c("KmknnIndex", "KmknnParam"), .QUERYKNN_GENERATOR_NOX(queryKmknn))

#' @export
setMethod("queryKNN", c("missing", "VptreeParam"), .QUERYKNN_GENERATOR(queryVptree))

#' @export
setMethod("queryKNN", c("VptreeIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryVptree))

#' @export
setMethod("queryKNN", c("VptreeIndex", "VptreeParam"), .QUERYKNN_GENERATOR_NOX(queryVptree))

#' @export
setMethod("queryKNN", c("missing", "AnnoyParam"), .QUERYKNN_GENERATOR(queryAnnoy))

#' @export
setMethod("queryKNN", c("AnnoyIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryAnnoy))

#' @export
setMethod("queryKNN", c("AnnoyIndex", "AnnoyParam"), .QUERYKNN_GENERATOR_NOX(queryAnnoy))

#' @export
setMethod("queryKNN", c("missing", "HnswParam"), .QUERYKNN_GENERATOR(queryHnsw))

#' @export
setMethod("queryKNN", c("HnswIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryHnsw))

#' @export
setMethod("queryKNN", c("HnswIndex", "HnswParam"), .QUERYKNN_GENERATOR_NOX(queryHnsw))

#' @export
setMethod("queryKNN", c("missing", "ExhaustiveParam"), .QUERYKNN_GENERATOR(queryExhaustive))

#' @export
setMethod("queryKNN", c("ExhaustiveIndex", "missing"), .QUERYKNN_GENERATOR_NOX(queryExhaustive))

#' @export
setMethod("queryKNN", c("ExhaustiveIndex", "ExhaustiveParam"), .QUERYKNN_GENERATOR_NOX(queryExhaustive))
