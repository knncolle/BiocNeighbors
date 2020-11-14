#' The ExhaustiveIndex class
#'
#' A class to hold the data for exact nearest neighbor identification. 
#'
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#'
#' @details 
#' Users should never need to call the constructor explicitly, but should generate 
#' instances of ExhaustiveIndex classes with \code{\link{buildExhaustive}}.
#'
#' Users can get values from an ExhaustiveIndex object with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be extracted in this manner.
#'
#' @return An ExhaustiveIndex object. 
#'
#' @examples
#' example(buildExhaustive)
#' out[['distance']]
#' bndistance(out)
#' 
#' @seealso
#' \code{\link{buildExhaustive}}, for the index construction.
#'
#' \linkS4class{BiocNeighborIndex}, for the parent class and its available methods.
#' 
#' @aliases ExhaustiveIndex-class
#' bnorder,ExhaustiveIndex-method 
#' @docType class
#' 
#' @export
#' @importFrom methods new
ExhaustiveIndex <- function(data, NAMES=NULL, distance="Euclidean") {
    new("ExhaustiveIndex", data=data, NAMES=NAMES, distance=distance)
}


#' @importFrom S4Vectors setValidity2
setValidity2("ExhaustiveIndex", function(object) {
    msg <- character(0)
    data <- bndata(object)
    order <- bnorder(object)

    if (length(order)!=ncol(data)) {
        msg <- c(msg, "number of observations is not consistent between 'data' and 'order'")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @exportMethod bnorder
setMethod("bnorder", "ExhaustiveIndex", function(x) seq_len(ncol(bndata(x))))

.find_exhaustive_args <- function(precomputed) {
    list()
}
