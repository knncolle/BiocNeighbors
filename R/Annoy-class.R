##################################
###### AnnoyParam methods ########
##################################

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

    if (length(object[['directory']])!=1L) {
        msg <- c(msg, "'directory' should be a string")
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
    cat(sprintf("directory: %s\n", object[['directory']]))
    cat(sprintf("search multiplier: %i\n", object[['search.mult']]))
})

setMethod("spill_args", "AnnoyParam", function(x) {
    list(ntrees=x[['ntrees']], directory=x[['directory']], 
        search.mult=x[['search.mult']], distance=bndistance(x))
})

##################################
###### AnnoyIndex methods ########
##################################

#' @export
#' @importFrom methods new
AnnoyIndex <- function(data, path, search.mult=50, NAMES=NULL, distance="Euclidean") {
    new("AnnoyIndex", data=data, path=path, NAMES=NAMES, distance=distance, search.mult=search.mult)
}

#' @importFrom S4Vectors setValidity2
setValidity2("AnnoyIndex", function(object) {
    msg <- character(0)

    path <- object[['path']]
    if (length(path)!=1L) {
        msg <- c(msg, "'path' should be a string")
    }

    search.mult <- object[['search.mult']]
    if (length(search.mult)!=1L || is.na(search.mult) || search.mult <= 1) {
        msg <- c(msg, "'search.mult' should be a numeric scalar greater than 1")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
setMethod("show", "AnnoyIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", object[['path']]))
    cat(sprintf("search multiplier: %i\n", object[['search.mult']]))
})

#' @export
AnnoyIndex_path <- function(x) {
    .Deprecated(new="x[['path']]")
    x@path
}

#' @export
AnnoyIndex_search_mult <- function(x) {
    .Deprecated(new="x[['search.mult']]")
    x@search.mult
}

#' @export
setMethod("bnorder", "AnnoyIndex", function(x) seq_len(ncol(bndata(x))) )
