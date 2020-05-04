#############################
##### HnswParam methods #####
#############################
#' The HnswParam class
#'
#' A class to hold parameters for the Hnsw algorithm for approximate nearest neighbor identification.
#'
#' @param nlinks Integer scalar, number of bi-directional links per element for index generation. 
#' @param ef.construction Integer scalar, size of the dynamic list for index generation.
#' @param directory String, the directory in which to save the index.
#' @param ef.search Integer scalar, size of the dynamic list for neighbor searching.
#' @param distance String, the distance metric to use.
#' Defaults to \code{"Euclidean"}.
#'
#' @return An HnswParam object.
#'
#' @details
#' The HnswParam class holds any parameters associated with running the HNSW algorithm.
#' This generally relates to building of the index - see \code{\link{buildHnsw}} for details. 
#'
#' @examples
#' out <- HnswParam()
#' HnswParam_nlinks(out) 
#' HnswParam_ef_construction(out) 
#' HnswParam_directory(out) 
#'
#' @seealso 
#' \code{\link{buildHnsw}}. 
#' @author Aaron Lun
#' @export
#' @importFrom methods new
HnswParam <- function(nlinks=16, ef.construction=200, directory=tempdir(), ef.search=10, distance="Euclidean") {
    new("HnswParam", nlinks=as.integer(nlinks), ef.construction=as.integer(ef.construction), dir=directory, ef.search=as.integer(ef.search), distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("HnswParam", function(object) {
    msg <- character(0)

    nlinks <- HnswParam_nlinks(object)
    if (length(nlinks) != 1L || nlinks <= 0L) {
        msg <- c(msg, "'nlinks' should be a positive integer scalar")
    }

    ef.construction <- HnswParam_ef_construction(object)
    if (length(ef.construction) != 1L || ef.construction <= 0L) {
        msg <- c(msg, "'ef.construction' should be a positive integer scalar")
    }

    dir <- HnswParam_directory(object)
    if (length(dir)!=1L) {
        msg <- c(msg, "'directory' should be a string")
    }

    ef.search <- HnswParam_ef_search(object)
    if (length(ef.search) != 1L || ef.search <= 0L) {
        msg <- c(msg, "'ef.search' should be a positive integer scalar")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
#' @rdname HnswParam
HnswParam_nlinks <- function(x) {
    x@nlinks
}

#' @export
#' @rdname HnswParam
HnswParam_ef_construction <- function(x) {
    x@ef.construction
}

#' @export
#' @rdname HnswParam
HnswParam_directory <- function(x) {
    x@dir
}

#' @export
#' @rdname HnswParam
HnswParam_ef_search <- function(x) {
    x@ef.search
}

#' @exportMethod show
#' @rdname HnswParam
#' @aliases show,HnswParam-method
setMethod("show", "HnswParam", function(object) {
    callNextMethod()
    cat(sprintf("nlinks: %i\n", HnswParam_nlinks(object)))
    cat(sprintf("EF construction: %i\n", HnswParam_ef_construction(object)))
    cat(sprintf("directory: %s\n", HnswParam_directory(object)))
    cat(sprintf("EF search: %i\n", HnswParam_ef_search(object)))
})

setMethod("spill_args", "HnswParam", function(x) {
    list(nlinks=HnswParam_nlinks(x), ef.construction=HnswParam_ef_construction(x), 
        directory=HnswParam_directory(x), ef.search=HnswParam_ef_search(x), distance=bndistance(x))
})

#############################
##### HnswIndex methods #####
#############################
#' The HnswIndex class
#'
#' A class to hold indexing structures for the HNSW algorithm for approximate nearest neighbor identification.
#'
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param path A string specifying the path to the index file.
#' @param ef.search Integer scalar specifying the size of the dynamic list at run time.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#'
#' @return An HnswIndex object.
#'
#' @details
#' The HnswIndex class holds the indexing structure required to run the HNSW algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of HnswIndex classes with \code{\link{buildHnsw}}.
#'
#' @examples
#' out <- example(buildHnsw)
#' str(HnswIndex_path(out))
#' str(HnswIndex_ef_search(out))
#'
#' @seealso 
#' \code{\link{buildHnsw}}. 
#' @author Aaron Lun


#' @export
#' @importFrom methods new
HnswIndex <- function(data, path, ef.search=10, NAMES=NULL, distance="Euclidean") {
    new("HnswIndex", data=data, path=path, ef.search=as.integer(ef.search), NAMES=NAMES, distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("HnswIndex", function(object) {
    msg <- character(0)

    path <- HnswIndex_path(object)
    if (length(path)!=1L) {
        msg <- c(msg, "'path' should be a string")
    }

    ef.search <- HnswParam_ef_search(object)
    if (length(ef.search) != 1L || ef.search <= 0L) {
        msg <- c(msg, "'ef.search' should be a positive integer scalar")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
#' @rdname HnswIndex
HnswIndex_path <- function(x) {
    x@path
}

#' @export
#' @rdname HnswIndex
HnswIndex_ef_search <- function(x) {
    x@ef.search
}

#' @exportMethod bnorder
#' @rdname HnswIndex
#' @aliases HnswIndex-method
setMethod("bnorder", "HnswIndex", function(x) seq_len(ncol(bndata(x))) )

#' @exportMethod show
#' @rdname HnswIndex
#' @aliases show,HnswIndex-method
setMethod("show", "HnswIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", HnswIndex_path(object)))
    cat(sprintf("EF search: %i\n", HnswIndex_ef_search(object)))
})
