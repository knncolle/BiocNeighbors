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
#' @details
#' The HnswIndex class holds the indexing structure required to run the HNSW algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of HnswIndex classes with \code{\link{buildHnsw}}.
#' 
#' Users can get values from an HnswIndex object with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be extracted in this manner.
#' 
#' @return An instance of the HnswIndex class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildHnsw}}, to build the index.
#' 
#' \linkS4class{BiocNeighborIndex}, for the parent class and its available methods.
#'
#' @examples
#' example(buildHnsw)
#' out[['path']]
#'
#' @aliases
#' HnswIndex-class
#' show,HnswIndex-method
#' HnswIndex_path
#' HnswIndex_ef_search
#' bnorder,HnswIndex-method
#' @docType class
#' 
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

.find_hnsw_args <- function(precomputed) {
    list(
        vals=bndata(precomputed),
        fname=precomputed[['path']],
        ef_search=precomputed[['ef.search']]
    )
}
