#' The AnnoyParam class
#'
#' A class to hold parameters for the Annoy algorithm for approximate nearest neighbor identification.
#' 
#' @param ntrees Integer scalar, number of trees to use for index generation.
#' @param directory String containing the path to the directory in which to save the index.
#' @param search.mult Numeric scalar, multiplier for the number of points to search.
#' @param distance String, the distance metric to use.
#' 
#' @details
#' The AnnoyParam class holds all parameters associated with running the Annoy algorithm.
#' Most of these parameters are used to build the index - see \code{\link{buildAnnoy}} for details.
#'
#' Users can get or set values with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be manipulated in this manner.
#' 
#' @return
#' An instance of the AnnoyParam class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildAnnoy}}, for the index construction.
#'
#' \code{\link{findAnnoy}} and related functions, for the actual search. 
#'
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- AnnoyParam())
#' out[['ntrees']]
#'
#' out[['ntrees']] <- 20L
#' out
#'
#' @aliases
#' AnnoyParam-class
#' show,AnnoyParam-method
#' AnnoyParam_ntrees
#' AnnoyParam_directory
#' [[,AnnoyParam-method
#' [[<-,AnnoyParam-method
#'
#' @docType class
#' 
#' @export
#' @importFrom methods new
AnnoyParam <- function(ntrees=50, directory=tempdir(), search.mult=ntrees, distance="Euclidean") {
    new("AnnoyParam", ntrees=as.integer(ntrees), dir=directory, distance=distance, search.mult=search.mult)
}

#' @importFrom S4Vectors setValidity2
setValidity2("AnnoyParam", function(object) {
    msg <- character(0)

    ntrees <- object[['ntrees']]
    if (length(ntrees) != 1L || ntrees <= 0L) {
        msg <- c(msg, "'ntrees' should be a positive integer scalar")
    }

    if (length(object[['dir']])!=1L) {
        msg <- c(msg, "'dir' should be a string")
    }

    search.mult <- object[['search.mult']]
    if (length(search.mult)!=1L || is.na(search.mult) || search.mult <= 1) {
        msg <- c(msg, "'search.mult' should be a numeric scalar greater than 1")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
AnnoyParam_ntrees <- function(x) {
    .Deprecated(new="x[['ntrees']]")
    x@ntrees
}

#' @export
AnnoyParam_directory <- function(x) {
    .Deprecated(new="x[['directory']]")
    x@dir
}

#' @export
AnnoyParam_search_mult <- function(x) {
    .Deprecated(new="x[['search.mult']]")
    x@search.mult
}

#' @export
setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", object[['ntrees']]))
    cat(sprintf("directory: %s\n", object[['dir']]))
    cat(sprintf("search multiplier: %i\n", object[['search.mult']]))
})

setMethod("spill_args", "AnnoyParam", function(x) {
    list(ntrees=x[['ntrees']], directory=x[['dir']], 
        search.mult=x[['search.mult']], distance=bndistance(x))
})

#' @export
setMethod("[[", "AnnoyParam", function(x, i, j, ...) {
    if (i=="directory") i <- "dir"
    callNextMethod()
})

#' @export
setReplaceMethod("[[", "AnnoyParam", function(x, i, j, ..., value) {
    if (i=="directory") i <- "dir"
    callNextMethod()
})

