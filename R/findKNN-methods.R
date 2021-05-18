#' Find k-nearest neighbors
#'
#' Find the k-nearest neighbors for each point in a data set, using exact or approximate algorithms.
#' 
#' @param X A numeric data matrix where rows are points and columns are dimensions.
#' This can be missing if \code{BNINDEX} is supplied.
#' @param k An integer scalar specifying the number of nearest neighbors to search for.
#' @param ... Further arguments to pass to individual methods.
#' This is guaranteed to include \code{subset}, \code{get.index}, \code{get.distance}, \code{last},
#' \code{warn.ties}, \code{raw.index} and \code{BPPARAM}.
#' See \code{?"\link{findKNN-functions}"} for more details.
#' @param BNINDEX A \linkS4class{BiocNeighborIndex} object containing precomputed index information.
#' This can be missing if \code{X} and \code{BNPARAM} are supplied, see Details.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the algorithm to use.
#' This can be missing if \code{BNINDEX} is supplied, see Details.
#'
#' @return 
#' A list is returned containing \code{index}, an integer matrix of neighbor identities;
#' and \code{distance}, a numeric matrix of distances to those neighbors.
#' See \code{?"\link{findKNN-functions}"} for more details.
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
#' \code{\link{findExhaustive}},
#' \code{\link{findKmknn}},
#' \code{\link{findVptree}},
#' \code{\link{findAnnoy}}
#' and \code{\link{findHnsw}} for specific methods.
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' str(k.out <- findKNN(Y, k=10))
#' str(a.out <- findKNN(Y, k=10, BNPARAM=AnnoyParam()))
#' 
#' e.dex <- buildExhaustive(Y)
#' str(k.out2 <- findKNN(Y, k=10, BNINDEX=e.dex))
#' str(k.out3 <- findKNN(Y, k=10, BNINDEX=e.dex, BNPARAM=ExhaustiveParam()))
#' 
#' k.dex <- buildKmknn(Y)
#' str(k.out2 <- findKNN(Y, k=10, BNINDEX=k.dex))
#' str(k.out3 <- findKNN(Y, k=10, BNINDEX=k.dex, BNPARAM=KmknnParam()))
#' 
#' a.dex <- buildAnnoy(Y)
#' str(a.out2 <- findKNN(Y, k=10, BNINDEX=a.dex))
#' str(a.out3 <- findKNN(Y, k=10, BNINDEX=a.dex, BNPARAM=AnnoyParam()))
#'
#' @aliases
#' findKNN,missing,missing-method
#' 
#' findKNN,missing,ExhaustiveParam-method
#' findKNN,ExhaustiveIndex,missing-method
#' findKNN,ExhaustiveIndex,ExhaustiveParam-method
#' 
#' findKNN,missing,KmknnParam-method
#' findKNN,KmknnIndex,missing-method
#' findKNN,KmknnIndex,KmknnParam-method
#' 
#' findKNN,missing,VptreeParam-method
#' findKNN,VptreeIndex,missing-method
#' findKNN,VptreeIndex,VptreeParam-method
#' 
#' findKNN,missing,AnnoyParam-method
#' findKNN,AnnoyIndex,missing-method
#' findKNN,AnnoyIndex,AnnoyParam-method
#' 
#' findKNN,missing,HnswParam-method
#' findKNN,HnswIndex,missing-method
#' findKNN,HnswIndex,HnswParam-method
#'
#' @docType methods
#' @name findKNN-methods
NULL

##############
# S4 Factory #
##############

#' @importFrom BiocParallel SerialParam
.FINDKNN_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, k, ..., BNINDEX, BNPARAM) {
        do.call(FUN, c(list(X=X, k=k, ...), ARGS(BNPARAM)))
    }
}

#' @importFrom BiocParallel SerialParam
.FINDKNN_GENERATOR_NOX <- function(FUN) {
    function(X, k, ..., BNINDEX, BNPARAM) {
        FUN(k=k, ..., precomputed=BNINDEX)
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
setMethod("findKNN", c("missing", "ExhaustiveParam"), .FINDKNN_GENERATOR(findExhaustive))

#' @export
setMethod("findKNN", c("ExhaustiveIndex", "ExhaustiveParam"), .FINDKNN_GENERATOR_NOX(findExhaustive))

#' @export
setMethod("findKNN", c("ExhaustiveIndex", "missing"), .FINDKNN_GENERATOR_NOX(findExhaustive))

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
