#######################################
###### ExhaustiveParam methods ########
#######################################

#' @importFrom methods new
#' @export
ExhaustiveParam <- function(..., distance="Euclidean") {
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
#'
#' @details 
#' Users should never need to call the constructor explicitly, but should generate 
#' instances of ExhaustiveIndex classes with \code{\link{buildExhaustive}}.
#'
#' @return An ExhaustiveIndex object. 
#'
#' @aliases ExhaustiveIndex-class
#' @importFrom methods new
#' @export
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


#' @aliases show,ExhaustiveIndex-method
#' @export
setMethod("show", "ExhaustiveIndex", function(object) {
    callNextMethod()
})

#' @aliases bnorder,ExhaustiveIndex-method
#' @export
setMethod("bnorder", "ExhaustiveIndex", function(x) seq_len(ncol(bndata(x))))

