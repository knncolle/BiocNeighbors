#' Find all neighbors in range
#'
#' Find all neighbors within a given distance for each point in a dataset.
#'
#' @param X A numeric data matrix where rows are points and columns are dimensions.
#' This can be missing if \code{BNINDEX} is supplied.
#' @param threshold A numeric scalar or vector specifying the maximum distance for considering neighbors.
#' @param ... Further arguments to pass to specific methods.
#' This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance} \code{BPPARAM} and \code{raw.index}.
#' See \code{?"\link{rangeFind-methods}"} for more details.
#' @param BNINDEX A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
#' This can be missing if \code{BNPARAM} is supplied, see Details.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
#' This can be missing if \code{BNINDEX} is supplied, see Details.
#'
#' @return List containing:
#' \code{index}, a list of integer vectors specifying the identities of the neighbors of each point;
#' and \code{distance}, a list of numeric vectors containing the distances to those neighbors.
#' See \code{?"\link{rangeFind-methods}"} for more details.
#'
#' @details
#' The class of \code{BNINDEX} and \code{BNPARAM} will determine the dispatch to specific functions.
#' Only one of these arguments needs to be defined to resolve dispatch.
#' However, if both are defined, they cannot specify different algorithms.
#' 
#' If \code{BNINDEX} is supplied, \code{X} does not need to be specified.
#' In fact, any value of \code{X} will be ignored as all necessary information for the search is already present in \code{BNINDEX}.
#' Similarly, any parameters in \code{BNPARAM} will be ignored.
#' 
#' If both \code{BNINDEX} and \code{BNPARAM} are missing, the function will default to the KMKNN algorithm by setting \code{BNPARAM=KmknnParam()}.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{rangeFindExhaustive}},
#' \code{\link{rangeFindKmknn}},
#' and \code{\link{rangeFindVptree}} for specific methods.
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' 
#' k.out <- findNeighbors(Y, threshold=3)
#' a.out <- findNeighbors(Y, threshold=3, BNPARAM=VptreeParam())
#' 
#' e.dex <- buildExhaustive(Y)
#' e.out2 <- findNeighbors(Y, threshold=3, BNINDEX=e.dex)
#' e.out3 <- findNeighbors(Y, threshold=3, BNINDEX=e.dex, BNPARAM=ExhaustiveParam())
#' 
#' k.dex <- buildKmknn(Y)
#' k.out2 <- findNeighbors(Y, threshold=3, BNINDEX=k.dex)
#' k.out3 <- findNeighbors(Y, threshold=3, BNINDEX=k.dex, BNPARAM=KmknnParam())
#' 
#' v.dex <- buildVptree(Y)
#' v.out2 <- findNeighbors(Y, threshold=3, BNINDEX=v.dex)
#' v.out3 <- findNeighbors(Y, threshold=3, BNINDEX=v.dex, BNPARAM=VptreeParam())
#'
#' @name findNeighbors
#' @docType methods
NULL

##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.FINDNEIGHBORS_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, threshold, ..., BNINDEX, BNPARAM) {
        do.call(FUN, c(list(X=X, threshold=threshold, ...), ARGS(BNPARAM)))
    }
}

#' @importFrom BiocParallel SerialParam
.FINDNEIGHBORS_GENERATOR_NOX <- function(FUN) {
    function(X, threshold, ..., BNINDEX, BNPARAM) {
        FUN(threshold=threshold, ..., precomputed=BNINDEX)
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
setMethod("findNeighbors", c("missing", "ExhaustiveParam"), .FINDNEIGHBORS_GENERATOR(rangeFindExhaustive))

#' @export
setMethod("findNeighbors", c("ExhaustiveIndex", "missing"), .FINDNEIGHBORS_GENERATOR_NOX(rangeFindExhaustive))

#' @export
setMethod("findNeighbors", c("ExhaustiveIndex", "ExhaustiveParam"), .FINDNEIGHBORS_GENERATOR_NOX(rangeFindExhaustive))

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
