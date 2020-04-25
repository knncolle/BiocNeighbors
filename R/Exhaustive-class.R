#######################################
###### ExhaustiveParam methods ########
#######################################

#' @export
#' @importFrom methods new
ExhaustiveParam <- function(..., distance="Euclidean") {
    new("ExhaustiveParam", distance=distance)
}

setMethod("spill_args", "ExhaustiveParam", function(x) {
    c(list(distance=bndistance(x)))
})

#######################################
###### ExhaustiveIndex methods ########
#######################################

#' @export
#' @importFrom methods new
ExhaustiveIndex <- function(data, order, NAMES=NULL, distance="Euclidean") {
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


#' @export
setMethod("show", "ExhaustiveIndex", function(object) {
    callNextMethod()
})


#' @export
setMethod("bnorder", "ExhaustiveIndex", function(x) seq_len(ncol(bndata(x))))

