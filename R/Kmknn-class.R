##################################
###### KmknnParam methods ########
##################################
#' The KmknnParam class
#'
#' A class to hold parameters for the KMKNN algorithm for exact nearest neighbor identification.
#'
#' @param ... Arguments to be passed to \code{\link{kmeans}}.
#' @param distance A string specifying the distance metric to use.
#' Defaults to \code{"Euclidean"}.
#'
#' @return A KmknnParam object.
#'
#' @details
#' The KmknnParam class holds any parameters associated with running the KMKNN algorithm.
#' Currently, this relates to tuning of the k-means step - see \code{\link{buildKmknn}} for details.
#'
#' @examples
#' out <- KmknnParam(iter.max=100)
#' KmknnParam_kmeans_args(out) 
#'
#' @seealso 
#' \code{\link{buildKmknn}}. 
#' @author Aaron Lun
#' @export
#' @importFrom methods new
KmknnParam <- function(..., distance="Euclidean") {
    new("KmknnParam", kmeans.args=list(...), distance=distance)
}

#' @export
KmknnParam_kmeans_args <- function(x) {
    x@kmeans.args
}

#' @exportMethod show
#' @aliases show,KmknnParam-method
#' @rdname KmknnParam
setMethod("show", "KmknnParam", function(object) {
    callNextMethod()

    all.args <- names(KmknnParam_kmeans_args(object))
    all.args[is.na(all.args)] <- ""
    N <- length(all.args)
    if (N >= 4L) all.args <- c(all.args[seq_len(3)], "...")

    cat(sprintf("kmeans args(%i): %s\n", N, paste(all.args, collapse=" ")))
})

setMethod("spill_args", "KmknnParam", function(x) {
    c(list(distance=bndistance(x)), KmknnParam_kmeans_args(x))
})

##################################
###### KmknnIndex methods ########
##################################
#' The KmknnIndex class
#'
#' A class to hold indexing structures for the KMKNN algorithm for exact nearest neighbor identification.
#'
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param centers A numeric matrix with clusters in columns and dimensions in rows.
#' @param info A list of statistics for each cluster.
#' @param order An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#'
#' @return A KmknnIndex object.
#'
#' @details
#' The KmknnIndex class holds the indexing structure required to run the KMKNN algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of KmknnIndex classes with \code{\link{buildKmknn}}.
#'
#' @examples
#' out <- KmknnIndex(data, centers, info, order, NAMES=NULL, distance="Euclidean")
#' str(KmknnIndex_cluster_centers(out))
#' str(KmknnIndex_cluster_info(out))
#'
#' @seealso 
#' \code{\link{buildKmknn}}. 
#' @author Aaron Lun
#' @export
#' @importFrom methods new
KmknnIndex <- function(data, centers, info, order, NAMES=NULL, distance="Euclidean") {
    new("KmknnIndex", data=data, centers=centers, info=info, order=order, NAMES=NAMES, distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("KmknnIndex", function(object) {
    msg <- character(0)

    data <- bndata(object)
    centers <- KmknnIndex_cluster_centers(object)
    if (nrow(data)!=nrow(centers)) {
        msg <- c(msg, "dimensionality is not consistent between 'data' and 'centers") 
    }

    info <- KmknnIndex_cluster_info(object)
    if (length(info)!=ncol(centers)) {
        msg <- c(msg, "number of clusters is not consistent between 'centers' and 'info'")
    }

    order <- bnorder(object)
    if (length(order)!=ncol(data)) {
        msg <- c(msg, "number of observations is not consistent between 'data' and 'order'")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @exportMethod show
#' @aliases show,KmknnIndex-method
#' @rdname KmknnIndex
setMethod("show", "KmknnIndex", function(object) {
    callNextMethod()
    cat(sprintf("clusters: %i\n", ncol(KmknnIndex_cluster_centers(object))))
})

#' @export        
#' @rdname KmknnIndex
KmknnIndex_cluster_centers <- function(x) {
    x@centers
}

#' @export
#' @rdname KmknnIndex
KmknnIndex_cluster_info <- function(x) {
    x@info
}

#' @exportMethod bnorder
#' @aliases KmknnIndex-method
#' @rdname KmknnIndex
setMethod("bnorder", "KmknnIndex", function(x) x@order)
