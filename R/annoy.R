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
#' buildIndex,AnnoyParam-method
#'
#' @docType class
#' 
#' @export
#' @importFrom methods new
AnnoyParam <- function(ntrees=50, search.mult=ntrees, distance="Euclidean") {
    new("AnnoyParam", ntrees=as.integer(ntrees), distance=distance, search.mult=search.mult)
}

#' @importFrom S4Vectors setValidity2
setValidity2("AnnoyParam", function(object) {
    msg <- character(0)

    ntrees <- object[['ntrees']]
    if (length(ntrees) != 1L || ntrees <= 0L) {
        msg <- c(msg, "'ntrees' should be a positive integer scalar")
    }

    search.mult <- object[['search.mult']]
    if (length(search.mult)!=1L || is.na(search.mult) || search.mult <= 1) {
        msg <- c(msg, "'search.mult' should be a numeric scalar greater than 1")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
setMethod("show", "AnnoyParam", function(object) {
    callNextMethod()
    cat(sprintf("ntrees: %i\n", object[['ntrees']]))
    cat(sprintf("search.mult: %i\n", object[['search.mult']]))
})

#' @export
setMethod("buildIndex", "AnnoyParam", function(X, ..., BNPARAM) {
    build_annoy(X, num_trees=BNPARAM@ntrees, distance=BNPARAM@distance)
})
