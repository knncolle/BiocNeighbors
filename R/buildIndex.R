#' Build a nearest-neighbor index
#'
#' Build indices for nearest-neighbor searching with different algorithms.
#' 
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param ... Further arguments to be passed to individual methods.
#' This is guaranteed to include \code{transposed}.
#' @param BNPARAM A \linkS4class{BiocNeighborParam} object specifying the type of index to be constructed.
#' This defaults to a \linkS4class{KmknnParam} object if no argument is supplied.
#' 
#' @details
#' Supplying a \linkS4class{KmknnParam} object as \code{BNPARAM} will dispatch to \code{\link{buildKmknn}}.
#' 
#' Supplying a \linkS4class{VptreeParam} object as \code{BNPARAM} will dispatch to \code{\link{buildVptree}}.
#' 
#' Supplying an \linkS4class{AnnoyParam} object as \code{BNPARAM} will dispatch to \code{\link{buildAnnoy}}.
#' 
#' Supplying an \linkS4class{HnswParam} object as \code{BNPARAM} will dispatch to \code{\link{buildHnsw}}.
#' 
#' @return
#' An instance of a \linkS4class{BiocNeighborIndex} subclass, containing indexing structures for the specified algorithm.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildKmknn}},
#' \code{\link{buildVptree}},
#' \code{\link{buildAnnoy}} 
#' and \code{\link{buildHnsw}} for specific methods. 
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' (k.out <- buildIndex(Y))
#' (a.out <- buildIndex(Y, BNPARAM=AnnoyParam()))
#'
#' @aliases
#' buildIndex,missing-method
#' buildIndex,KmknnParam-method
#' buildIndex,VptreeParam-method
#' buildIndex,AnnoyParam-method
#' buildIndex,HnswParam-method
#'
#' @name buildIndex
NULL

.BUILDINDEX_GENERATOR <- function(FUN, ARGS=spill_args) {
    function(X, transposed=FALSE, ..., BNPARAM) {    
        do.call(FUN, c(list(X=X, transposed=transposed, ...), ARGS(BNPARAM)))
    }
}

#' @export
setMethod("buildIndex", "missing", .BUILDINDEX_GENERATOR(buildIndex, .default_param))

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
