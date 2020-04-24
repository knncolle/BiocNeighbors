.BUILDINDEX_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, transposed=FALSE, ..., BNPARAM) {    
        do.call(FUN, c(list(X=X, transposed=transposed, ...), ARGS(BNPARAM)))
    }
}

####################
# Default dispatch #
####################

#' @export
setMethod("buildIndex", "missing", .BUILDINDEX_GENERATOR(buildIndex, .default_param))

####################
# Specific methods #
####################

#' @export
setMethod("buildIndex", "KmknnParam", .BUILDINDEX_GENERATOR(buildKmknn))

#' @export
setMethod("buildIndex", "VptreeParam", .BUILDINDEX_GENERATOR(buildVptree))

#' @export
setMethod("buildIndex", "AnnoyParam", .BUILDINDEX_GENERATOR(buildAnnoy))

#' @export
setMethod("buildIndex", "HnswParam", .BUILDINDEX_GENERATOR(buildHnsw))

#' @export
setMethod("buildIndex", "ExhaustiveParam", .BUILDINDEX_GENERATOR(buildExhaustive))
