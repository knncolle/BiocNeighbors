#' @export
#' @importFrom methods show 
setMethod("show", "BiocNeighborIndex", function(object) {
    cat(sprintf("class: %s\n", class(object)))
    cat(sprintf("dim: %i %i\n", nrow(object), ncol(object)))
    cat(sprintf("distance: %s\n", bndistance(object)))
})

#' @export
setMethod("bndistance", "BiocNeighborIndex", function(x) x@distance)

#' @export
setMethod("dimnames", "BiocNeighborIndex", function(x) {
    list(x@NAMES, NULL)
})

#' @export
setMethod("bndata", "BiocNeighborIndex", function(x) x@data)

#' @export
setMethod("dim", "BiocNeighborIndex", function(x) rev(dim(bndata(x))) ) # reversed, as matrix was transposed.

#' @importFrom S4Vectors setValidity2
setValidity2("BiocNeighborIndex", function(object) {
    msg <- character(0) 

    NAMES <- rownames(object)
    if (!is.null(NAMES) && length(NAMES)!=nrow(object)) {
        msg <- c(msg, "length of non-NULL 'NAMES' is not equal to the number of rows")
    }

    if (length(bndistance(object))!=1L) {
        msg <- c(msg, "'distance' must be a string")
    }
    
    if (length(msg)) return(msg)
    return(TRUE)
})
