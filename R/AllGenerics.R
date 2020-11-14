#' @export
#' @rdname buildIndex
setGeneric("buildIndex", signature=c("BNPARAM"), 
    function(X, ..., BNPARAM) 
        standardGeneric("buildIndex")
)

#' @export
#' @rdname findKNN-methods
setGeneric("findKNN", signature=c("BNINDEX", "BNPARAM"), 
    function(X, k, ..., BNINDEX, BNPARAM) 
        standardGeneric("findKNN")
)

#' @export
#' @rdname queryKNN-methods
setGeneric("queryKNN", signature=c("BNINDEX", "BNPARAM"), 
    function(X, query, k, ..., BNINDEX, BNPARAM) 
        standardGeneric("queryKNN")
)

#' @export
#' @rdname findNeighbors-methods
setGeneric("findNeighbors", signature=c("BNINDEX", "BNPARAM"), 
    function(X, threshold, ..., BNINDEX, BNPARAM)
        standardGeneric("findNeighbors")
)

#' @export
#' @rdname queryNeighbors-methods
setGeneric("queryNeighbors", signature=c("BNINDEX", "BNPARAM"), 
    function(X, query, threshold, ..., BNINDEX, BNPARAM)
        standardGeneric("queryNeighbors")
)

#' @export
setGeneric("bnorder", function(x) standardGeneric("bnorder"))

#' @export
setGeneric("bndata", function(x) standardGeneric("bndata"))

#' @export
setGeneric("bndistance", function(x) standardGeneric("bndistance"))

# Generic purely for internal use, to help in defining other S4 methods.
setGeneric("spill_args", function(x) standardGeneric("spill_args"))
