#######################################
###### ExhaustiveParam methods ########
#######################################

#' The ExhaustiveParam class
#'
#' A class to hold parameters for the exhaustive algorithm for exact nearest neighbor identification.
#' 
#' @param distance A string specifying the distance metric to use.
#' 
#' @return
#' The \code{ExhaustiveParam} constructor will return an instance of the ExhaustiveParam class.
#' 
#' @author
#' Allison Vuong
#' 
#' @seealso
#' \code{\link{buildExhaustive}} 
#' 
#' @examples
#' out <- ExhaustiveParam()
#'
#' @export
#' @importFrom methods new
#' 
#' @aliases ExhaustiveParam-class
ExhaustiveParam <- function(distance="Euclidean") {
    new("ExhaustiveParam", distance=distance)
}

setMethod("spill_args", "ExhaustiveParam", function(x) {
    c(list(distance=bndistance(x)))
})

#######################################
###### ExhaustiveIndex methods ########
#######################################

#' The ExhaustiveIndex class
#'
#' A class to hold the data for exact nearest neighbor identification. 
#'
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#' Defaults to \code{"Euclidean"}.
#'
#' @details 
#' Users should never need to call the constructor explicitly, but should generate 
#' instances of ExhaustiveIndex classes with \code{\link{buildExhaustive}}.
#'
#' @return An ExhaustiveIndex object. 
#'
#' @examples
#' example(buildExhaustive)
#' 
#' @export
#' @aliases ExhaustiveIndex-class
#' 
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
#' @rdname ExhaustiveIndex
#' @aliases bnorder,ExhaustiveIndex-method
setMethod("bnorder", "ExhaustiveIndex", function(x) seq_len(ncol(bndata(x))))
