#' @export
setGeneric("buildIndex", signature=c("BNPARAM"), function(..., BNPARAM=NULL) standardGeneric("buildIndex"))

#' @export
setGeneric("findKNN", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX=NULL, BNPARAM=NULL) standardGeneric("findKNN"))

#' @export
setGeneric("queryKNN", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX=NULL, BNPARAM=NULL) standardGeneric("queryKNN"))

#' @export
setGeneric("findNeighbors", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX=NULL, BNPARAM=NULL) standardGeneric("findNeighbors"))

#' @export
setGeneric("queryNeighbors", signature=c("BNINDEX", "BNPARAM"), function(..., BNINDEX=NULL, BNPARAM=NULL) standardGeneric("queryNeighbors"))

#' @export
setGeneric("bnorder", function(x) standardGeneric("bnorder"))

#' @export
setGeneric("bndata", function(x) standardGeneric("bndata"))

#' @export
setGeneric("bndistance", function(x) standardGeneric("bndistance"))
