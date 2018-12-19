#' @export
#' @importFrom methods show 
setMethod("show", "BiocNeighborParam", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("distance: %s\n", bndistance(object)))
})

#' @export
setMethod("bndistance", "BiocNeighborParam", function(x) x@distance)

#' @importFrom S4Vectors setValidity2
setValidity2("BiocNeighborParam", function(object) {
    msg <- character(0) 

    if (length(bndistance(object))!=1L) {
        msg <- c(msg, "'distance' must be a string")
    }
    
    if (length(msg)) return(msg)
    return(TRUE)
})
