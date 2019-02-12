#############################
##### HnswParam methods #####
#############################

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
HnswParam_nlinks <- function(x) {
    x@nlinks
}

#' @export
HnswParam_ef_construction <- function(x) {
    x@ef.construction
}

#' @export
HnswParam_directory <- function(x) {
    x@dir
}

#' @export
HnswParam_ef_search <- function(x) {
    x@ef.search
}

#' @export
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
HnswIndex_path <- function(x) {
    x@path
}

#' @export
HnswIndex_ef_search <- function(x) {
    x@ef.search
}

#' @export
setMethod("bnorder", "HnswIndex", function(x) seq_len(ncol(bndata(x))) )

#' @export
setMethod("show", "HnswIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", HnswIndex_path(object)))
    cat(sprintf("EF search: %i\n", HnswIndex_ef_search(object)))
})
