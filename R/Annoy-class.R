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

    ntrees <- AnnoyParam_ntrees(object)
    if (length(ntrees) != 1L || ntrees <= 0L) {
        msg <- c(msg, "'ntrees' should be a positive integer scalar")
    }

    if (length(AnnoyParam_directory(object))!=1L) {
        msg <- c(msg, "'directory' should be a string")
    }

    search.mult <- AnnoyParam_search_mult(object)
    if (length(search.mult)!=1L || is.na(search.mult) || search.mult <= 1) {
        msg <- c(msg, "'search.mult' should be a numeric scalar greater than 1")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
AnnoyParam_ntrees <- function(x) {
    x@ntrees
}

#' @export
AnnoyParam_directory <- function(x) {
    x@dir
}

#' @export
AnnoyParam_search_mult <- function(x) {
    x@search.mult
}

#' @export
setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", AnnoyParam_ntrees(object)))
    cat(sprintf("directory: %s\n", AnnoyParam_directory(object)))
    cat(sprintf("search multiplier: %i\n", AnnoyParam_search_mult(object)))
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

    path <- AnnoyIndex_path(object)
    if (length(path)!=1L) {
        msg <- c(msg, "'path' should be a string")
    }

    search.mult <- AnnoyIndex_search_mult(object)
    if (length(search.mult)!=1L || is.na(search.mult) || search.mult <= 1) {
        msg <- c(msg, "'search.mult' should be a numeric scalar greater than 1")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
setMethod("show", "AnnoyIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", AnnoyIndex_path(object)))
    cat(sprintf("search multiplier: %i\n", AnnoyIndex_search_mult(object)))
})

#' @export
AnnoyIndex_path <- function(x) {
    x@path
}

#' @export
AnnoyIndex_search_mult <- function(x) {
    x@search.mult
}

#' @export
setMethod("bnorder", "AnnoyIndex", function(x) seq_len(ncol(bndata(x))) )
