#' The ExhaustiveParam class
#'
#' A class to hold parameters for the exhaustive algorithm for exact nearest neighbor identification.
#' 
#' @param distance A string specifying the distance metric to use.
#' 
##' @return
#' An instance of the ExhaustiveParam class.
#' 
#' @author
#' Allison Vuong
#' 
#' @seealso
#' \code{\link{buildExhaustive}}, for the index construction.
#'
#' \code{\link{findExhaustive}} and related functions, for the actual search. 
#'
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- ExhaustiveParam())
#'
#' @aliases ExhaustiveParam-class
#' @docType class
#' 
#' @export
#' @importFrom methods new
ExhaustiveParam <- function(distance="Euclidean") {
    new("ExhaustiveParam", distance=distance)
}

setMethod("spill_args", "ExhaustiveParam", function(x) {
    c(list(distance=bndistance(x)))
})
