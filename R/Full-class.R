##################################
###### FullParam methods ########
##################################

#' @export
#' @importFrom methods new
FullParam <- function(..., distance="Euclidean") {
    new("FullParam", distance=distance)
}

#' @export
setMethod("show", "FullParam", function(object) {
    callNextMethod()
    # TODO: FILL ME
    all.args[is.na(all.args)] <- ""
    N <- length(all.args)
    if (N >= 4L) all.args <- c(all.args[seq_len(3)], "...")

})


setMethod("spill_args", "FullParam", function(x) {
    c(list(distance=bndistance(x)))
})

##################################
###### FullIndex methods ########
##################################

#' @export
#' @importFrom methods new
FullIndex <- function(data, order, NAMES=NULL, distance="Euclidean") {
    new("FullIndex", data=data, order=order, NAMES=NAMES, distance=distance)
}


#' @importFrom S4Vectors setValidity2
setValidity2("FullIndex", function(object) {
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
setMethod("show", "FullIndex", function(object) {
    callNextMethod()
    # TODO: Fill me
})


#' @export
setMethod("bnorder", "FullIndex", function(x) x@order)

