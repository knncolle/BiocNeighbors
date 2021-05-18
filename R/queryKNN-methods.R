#' Query k-nearest neighbors
#'
#' Find the k-nearest neighbors in one data set for each point in another query data set, using exact or approximate algorithms.
#' 
#' @inheritParams findKNN-methods
#' @param query A numeric query matrix where rows are points and columns are dimensions.
#' @param ... Further arguments to pass to specific methods.
#' This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance}, \code{last}, \code{transposed},
#' \code{warn.ties}, \code{raw.index} and \code{BPPARAM}.
#' See \code{?"\link{queryKNN-functions}"} for more details.
#' 
#' @return
#' A list is returned containing \code{index}, an integer matrix of neighbor identities;
#' and \code{distance}, a numeric matrix of distances to those neighbors.
#' See \code{?"\link{queryKNN-functions}"} for more details.
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
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{queryExhaustive}},
#' \code{\link{queryKmknn}},
#' \code{\link{queryVptree}},
#' \code{\link{queryAnnoy}}
#' and \code{\link{queryHnsw}} for specific methods.
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' Z <- matrix(rnorm(10000), ncol=20)
#' str(k.out <- queryKNN(Y, Z, k=10))
#' str(a.out <- queryKNN(Y, Z, k=10, BNPARAM=AnnoyParam()))
#' 
#' e.dex <- buildExhaustive(Y)
#' str(k.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=e.dex))
#' str(k.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=e.dex, BNPARAM=ExhaustiveParam()))
#' 
#' k.dex <- buildKmknn(Y)
#' str(k.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=k.dex))
#' str(k.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))
#' 
#' a.dex <- buildAnnoy(Y)
#' str(a.out2 <- queryKNN(Y,Z,  k=10, BNINDEX=a.dex))
#' str(a.out3 <- queryKNN(Y,Z,  k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
#'
#' @name queryKNN-methods
#' @docType methods
#' 
#' @aliases
#' queryKNN,missing,missing-method
#' 
#' queryKNN,missing,ExhaustiveParam-method
#' queryKNN,ExhaustiveIndex,missing-method
#' queryKNN,ExhaustiveIndex,ExhaustiveParam-method
#' 
#' queryKNN,missing,KmknnParam-method
#' queryKNN,KmknnIndex,missing-method
#' queryKNN,KmknnIndex,KmknnParam-method
#' 
#' queryKNN,missing,VptreeParam-method
#' queryKNN,VptreeIndex,missing-method
#' queryKNN,VptreeIndex,VptreeParam-method
#' 
#' queryKNN,missing,AnnoyParam-method
#' queryKNN,AnnoyIndex,missing-method
#' queryKNN,AnnoyIndex,AnnoyParam-method
#' 
#' queryKNN,missing,HnswParam-method
#' queryKNN,HnswIndex,missing-method
#' queryKNN,HnswIndex,HnswParam-method
NULL

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
