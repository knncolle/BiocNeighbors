#' Query all neighbors
#'
#' Find all neighbors in one data set that are in range of each point in another query data set.
#'
#' @param X A numeric data matrix where rows are points and columns are dimensions.
#' This can be missing if \code{BNINDEX} is supplied.
#' @param query A numeric query matrix where rows are points and columns are dimensions.
#' @param threshold A numeric scalar or vector specifying the maximum distance for considering neighbors.
#' @param ... Further arguments to pass to specific methods.
#' This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance} \code{BPPARAM} and \code{raw.index}.
#' See \code{?"\link{rangeQuery-methods}"} for more details.
#' @param BNINDEX A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
#' This can be missing if \code{BNPARAM} is supplied, see Details.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
#' This can be missing if \code{BNINDEX} is supplied, see Details.
#'
#' @return List containing:
#' \code{index}, a list of integer vectors specifying the identities of the neighbors of each point;
#' and \code{distance}, a list of numeric vectors containing the distances to those neighbors.
#' See \code{?"\link{rangeQuery-methods}"} for more details.
#'
#' @details
#' The class of \code{BNINDEX} and \code{BNPARAM} will determine dispatch to specific methods.
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
#' \code{\link{rangeQueryExhaustive}},
#' \code{\link{rangeQueryKmknn}}, and
#' \code{\link{rangeQueryVptree}} for specific methods.
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(10000), ncol=20)
#'
#' k.out <- queryNeighbors(Y, Z, threshold=3)
#' v.out <- queryNeighbors(Y, Z, threshold=3, BNPARAM=VptreeParam())
#' 
#' e.dex <- buildExhaustive(Y)
#' e.out2 <- queryNeighbors(Y, Z, threshold=3, BNINDEX=e.dex)
#' e.out3 <- queryNeighbors(Y, Z, threshold=3, BNINDEX=e.dex, BNPARAM=ExhaustiveParam())
#' 
#' k.dex <- buildKmknn(Y)
#' k.out2 <- queryNeighbors(Y, Z, threshold=3, BNINDEX=k.dex)
#' k.out3 <- queryNeighbors(Y, Z, threshold=3, BNINDEX=k.dex, BNPARAM=KmknnParam())
#' 
#' v.dex <- buildVptree(Y)
#' v.out2 <- queryNeighbors(Y, Z, threshold=3, BNINDEX=v.dex)
#' v.out3 <- queryNeighbors(Y, Z, threshold=3, BNINDEX=v.dex, BNPARAM=VptreeParam())
#'
#' @name queryNeighbors
#' @docType methods
NULL

##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.QUERYNEIGHBORS_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, query, threshold, ..., BNINDEX, BNPARAM) {
        do.call(FUN, c(list(X=X, query=query, threshold=threshold, ...), ARGS(BNPARAM)))
    }
}

#' @importFrom BiocParallel SerialParam
.QUERYNEIGHBORS_GENERATOR_NOX <- function(FUN) {
    function(X, query, threshold, ..., BNINDEX, BNPARAM) {
        FUN(query=query, threshold=threshold, ..., precomputed=BNINDEX)
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
setMethod("queryNeighbors", c("missing", "ExhaustiveParam"), .QUERYNEIGHBORS_GENERATOR(rangeQueryExhaustive))

#' @export
setMethod("queryNeighbors", c("ExhaustiveIndex", "missing"), .QUERYNEIGHBORS_GENERATOR_NOX(rangeQueryExhaustive))

#' @export
setMethod("queryNeighbors", c("ExhaustiveIndex", "ExhaustiveParam"), .QUERYNEIGHBORS_GENERATOR_NOX(rangeQueryExhaustive))

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
