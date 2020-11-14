#' The HnswParam class
#'
#' A class to hold parameters for the Hnsw algorithm for approximate nearest neighbor identification.
#' 
#' @param nlinks Integer scalar, number of bi-directional links per element for index generation.
#' @param ef.construction Integer scalar, size of the dynamic list for index generation.
#' @param directory String specifying the directory in which to save the index.
#' @param ef.search Integer scalar, size of the dynamic list for neighbor searching.
#' @param distance A string specifying the distance metric to use.
#' 
#' @details
#' The HnswParam class holds any parameters associated with running the HNSW algorithm.
#' This generally relates to building of the index - see \code{\link{buildHnsw}} for details.
#'
#' Users can get or set values with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be manipulated in this manner.
#' 
#' @return
#' An instance of the HnswParam class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildHnsw}}, for the index construction.
#'
#' \code{\link{findHnsw}} and related functions, for the actual search. 
#'
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- HnswParam())
#' out[['nlinks']]
#'
#' out[['nlinks']] <- 20L
#' out
#'
#' @aliases
#' HnswParam-class
#' show,HnswParam-method
#' HnswParam_nlinks
#' HnswParam_ef_construction
#' HnswParam_directory
#' HnswParam_ef_search
#' [[,HnswParam-method
#' [[<-,HnswParam-method
#' @docType class
#'
#' @export
#' @importFrom methods new
HnswParam <- function(nlinks=16, ef.construction=200, directory=tempdir(), ef.search=10, distance="Euclidean") {
    new("HnswParam", nlinks=as.integer(nlinks), ef.construction=as.integer(ef.construction), dir=directory, ef.search=as.integer(ef.search), distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("HnswParam", function(object) {
    msg <- character(0)

    nlinks <- object[['nlinks']]
    if (length(nlinks) != 1L || nlinks <= 0L) {
        msg <- c(msg, "'nlinks' should be a positive integer scalar")
    }

    ef.construction <- object[['ef.construction']]
    if (length(ef.construction) != 1L || ef.construction <= 0L) {
        msg <- c(msg, "'ef.construction' should be a positive integer scalar")
    }

    dir <- object[['dir']]
    if (length(dir)!=1L) {
        msg <- c(msg, "'dir' should be a string")
    }

    ef.search <- object[['ef.search']]
    if (length(ef.search) != 1L || ef.search <= 0L) {
        msg <- c(msg, "'ef.search' should be a positive integer scalar")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
HnswParam_nlinks <- function(x) {
    x@nlinks
}

#' @export
HnswParam_ef_construction <- function(x) {
    .Deprecated(new="x[['ef.construction']]")
    x@ef.construction
}

#' @export
HnswParam_directory <- function(x) {
    .Deprecated(new="x[['directory']]")
    x@dir
}

#' @export
HnswParam_ef_search <- function(x) {
    .Deprecated(new="x[['ef.search']]")
    x@ef.search
}

#' @export
setMethod("show", "HnswParam", function(object) {
    callNextMethod()
    cat(sprintf("nlinks: %i\n", object[['nlinks']]))
    cat(sprintf("EF construction: %i\n", object[['ef.construction']]))
    cat(sprintf("directory: %s\n", object[['dir']]))
    cat(sprintf("EF search: %i\n", object[['ef.search']]))
})

setMethod("spill_args", "HnswParam", function(x) {
    list(nlinks=x[['nlinks']], ef.construction=x[['ef.construction']], 
        directory=x[['dir']], ef.search=x[['ef.search']], distance=bndistance(x))
})

#' @export
setMethod("[[", "HnswParam", function(x, i, j, ...) {
    if (i=="directory") i <- "dir"
    callNextMethod()
})

#' @export
setReplaceMethod("[[", "HnswParam", function(x, i, j, ..., value) {
    if (i=="directory") i <- "dir"
    callNextMethod()
})
