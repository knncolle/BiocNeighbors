##################################
###### AnnoyParam methods ########
##################################
#' The AnnoyParam class
#'
#' A class to hold parameters for the Annoy algorithm for approximate nearest neighbor identification.
#'
#' @param ntrees Integer scalar, number of trees to use for index generation. 
#' @param directory String, the directory in which to save the index.
#' @param search.mult Numeric scalar, multiplier for the number of points to search.
#' @param distance String, the distance metric to use.
#' Defaults to \code{"Euclidean"}.
#'
#' @return An AnnoyParam object.
#'
#' @details
#' The AnnoyParam class holds any parameters associated with running the Annoy algorithm.
#' This generally relates to building of the index - see \code{\link{buildAnnoy}} for details. 
#'
#' @examples
#' out <- AnnoyParam()
#' AnnoyParam_ntrees(out) 
#' AnnoyParam_directory(out) 
#'
#' @seealso 
#' \code{\link{buildAnnoy}}. 
#' @author Aaron Lun
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
#' @rdname AnnoyParam
AnnoyParam_ntrees <- function(x) {
    x@ntrees
}

#' @export
#' @rdname AnnoyParam
AnnoyParam_directory <- function(x) {
    x@dir
}

#' @export
#' @rdname AnnoyParam
AnnoyParam_search_mult <- function(x) {
    x@search.mult
}

#' @exportMethod show
#' @rdname AnnoyParam
setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", AnnoyParam_ntrees(object)))
    cat(sprintf("directory: %s\n", AnnoyParam_directory(object)))
    cat(sprintf("search multiplier: %i\n", AnnoyParam_search_mult(object)))
})

setMethod("spill_args", "AnnoyParam", function(x) {
    list(ntrees=AnnoyParam_ntrees(x), directory=AnnoyParam_directory(x), 
        search.mult=AnnoyParam_search_mult(x), distance=bndistance(x))
})

##################################
###### AnnoyIndex methods ########
##################################
#' The AnnoyIndex class
#'
#' A class to hold indexing structures for the Annoy algorithm for approximate nearest neighbor identification.
#'
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param path A string specifying the path to the index file.
#' @param search.mult Numeric scalar, multiplier for the number of points to search.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#'
#' @return An AnnoyIndex object.
#'
#' @details
#' The AnnoyIndex class holds the indexing structure required to run the Annoy algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of AnnoyIndex classes with \code{\link{buildAnnoy}}.
#'
#' @examples
#' out <- example(buildAnnoy)
#' str(AnnoyIndex_path(out))
#' str(AnnoyIndex_search_mult(out))
#'
#' @seealso 
#' \code{\link{buildAnnoy}}. 
#' @author Aaron Lun
#'
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

#' @exportMethod show
#' @rdname AnnoyIndex
#' @aliases show,AnnoyIndex-method
setMethod("show", "AnnoyIndex", function(object) {
    callNextMethod()
    cat(sprintf("path: %s\n", AnnoyIndex_path(object)))
    cat(sprintf("search multiplier: %i\n", AnnoyIndex_search_mult(object)))
})

#' @export
#' @rdname AnnoyIndex
AnnoyIndex_path <- function(x) {
    x@path
}

#' @export
#' @rdname AnnoyIndex
AnnoyIndex_search_mult <- function(x) {
    x@search.mult
}

#' @exportMethod bnorder
#' @rdname AnnoyIndex
#' @aliases bnorder,AnnoyIndex-method
setMethod("bnorder", "AnnoyIndex", function(x) seq_len(ncol(bndata(x))) )
