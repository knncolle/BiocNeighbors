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
    .Deprecated(new="x[['dir']]")
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

    path <- object[['path']]
    if (length(path)!=1L) {
        msg <- c(msg, "'path' should be a string")
    }

    ef.search <- object[['ef.search']]
    if (length(ef.search) != 1L || ef.search <= 0L) {
        msg <- c(msg, "'ef.search' should be a positive integer scalar")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
HnswIndex_path <- function(x) {
    .Deprecated(new="x[['path']]")
    x@path
}

#' @export
HnswIndex_ef_search <- function(x) {
    .Deprecated(new="x[['ef.search']]")
    x@ef.search
}

#' @export
setMethod("bnorder", "HnswIndex", function(x) seq_len(ncol(bndata(x))) )

#' @export
setMethod("show", "HnswIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", object[['path']]))
    cat(sprintf("EF search: %i\n", object[['ef.search']]))
})
