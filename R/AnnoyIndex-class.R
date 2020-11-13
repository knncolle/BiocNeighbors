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
#' @details
#' The AnnoyIndex class holds the indexing structure required to run the Annoy algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of AnnoyIndex classes with \code{\link{buildAnnoy}}.
#'
#' Users can get values from an AnnoyIndex object with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be extracted in this manner.
#' 
#' @return 
#' An instance of the AnnoyIndex class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildAnnoy}}, for the index construction.
#'
#' \linkS4class{BiocNeighborIndex}, for the parent class and its available methods.
#' 
#' @examples
#' example(buildAnnoy)
#' out[['path']]
#' bndistance(out)
#' str(bndata(out))
#'
#' @aliases
#' AnnoyIndex
#' show,AnnoyIndex-method
#' bnorder,AnnoyIndex-method
#'
#' AnnoyIndex_path
#' AnnoyIndex_search_mult
#'
#' @docType class
#'
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

.find_annoy_args <- function(precomputed) {
    list(
        ndims=ncol(precomputed),
        fname=precomputed[['path']],
        search_mult=precomputed[['search.mult']]
    )
}
