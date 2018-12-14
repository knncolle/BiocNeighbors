#' @export
#' @importFrom methods new
HnswParam <- function(max.neighbors=16, ef.construction=200, directory=tempdir()) {
    new("HnswParam", max.neighbors=as.integer(max.neighbors), ef.construction=as.integer(ef.construction), dir=directory)
}

#' @importFrom S4Vectors setValidity2
setValidity2("HnswParam", function(object) {
    msg <- character(0)

    max.neighbors <- HnswParam_max_neighbors(object)
    if (length(max.neighbors) != 1L || max.neighbors <= 0L) {
        msg <- c(msg, "'max.neighbors' should be a positive integer scalar")
    }

    ef.construction <- HnswParam_ef_construction(object)
    if (length(ef.construction) != 1L || ef.construction <= 0L) {
        msg <- c(msg, "'ef.construction' should be a positive integer scalar")
    }

    dir <- HnswParam_directory(object)
    if (length(dir)!=1L) {
        msg <- c(msg, "'directory' should be a string")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
#' @importFrom methods new
HnswIndex <- function(data, path, NAMES=NULL) {
    new("HnswIndex", data=data, path=path, NAMES=NAMES)
}

#' @importFrom S4Vectors setValidity2
setValidity2("HnswIndex", function(object) {
    msg <- character(0)

    path <- HnswIndex_path(object)
    if (length(path)!=1L) {
        msg <- c(msg, "'path' should be a string")
    }

    NAMES <- rownames(object)
    if (!is.null(NAMES) && length(NAMES)!=nrow(object)) {
        msg <- c(msg, "length of non-NULL 'NAMES' is not equal to the number of rows")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})
