#' @export
#' @importFrom methods new
AnnoyParam <- function(ntrees=50, directory=tempdir()) {
    new("AnnoyParam", ntrees=as.integer(ntrees), dir=directory)
}

#' @importFrom S4Vectors setValidity2
setValidity2("AnnoyParam", function(object) {
    msg <- character(0)

    ntrees <- AnnoyParam_ntrees(object)
    if (length(ntrees) != 1L || ntrees <= 0L) {
        msg <- c(msg, "'ntrees' should be a positive integer scalar")
    }

    dir <- AnnoyParam_directory(object)
    if (length(dir)!=1L) {
        msg <- c(msg, "'directory' should be a string")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
#' @importFrom methods new
AnnoyIndex <- function(data, path, dim, NAMES=NULL) {
    new("AnnoyIndex", data=data, path=path, NAMES=NAMES)
}

#' @importFrom S4Vectors setValidity2
setValidity2("AnnoyIndex", function(object) {
    msg <- character(0)

    path <- AnnoyIndex_path(object)
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
