#' @export
setGeneric("buildIndex", signature=c("BNPARAM"), function(X, transposed=FALSE, ..., BNPARAM) standardGeneric("buildIndex"))

#' @export
#' @importFrom BiocParallel SerialParam
setGeneric("findKNN", signature=c("BNINDEX", "BNPARAM"), 
    function(X, k, subset=NULL, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), ..., BNINDEX, BNPARAM) 
        standardGeneric("findKNN")
)

#' @export
#' @importFrom BiocParallel SerialParam
setGeneric("queryKNN", signature=c("BNINDEX", "BNPARAM"), 
    function(X, query, k, subset=NULL, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), transposed=FALSE, ..., BNINDEX, BNPARAM) 
        standardGeneric("queryKNN")
)

#' @export
setGeneric("findNeighbors", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX, BNPARAM) standardGeneric("findNeighbors"))

#' @export
setGeneric("queryNeighbors", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX, BNPARAM) standardGeneric("queryNeighbors"))

#' @export
setGeneric("bnorder", function(x) standardGeneric("bnorder"))

#' @export
setGeneric("bndata", function(x) standardGeneric("bndata"))

#' @export
setGeneric("bndistance", function(x) standardGeneric("bndistance"))

# Generic purely for internal use, to help in defining other S4 methods.
setGeneric("spill_args", function(x) standardGeneric("spill_args"))
