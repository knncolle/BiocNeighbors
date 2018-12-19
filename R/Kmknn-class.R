##################################
###### KmknnParam methods ########
##################################

#' @export
#' @importFrom methods new
KmknnParam <- function(...) {
    new("KmknnParam", kmeans.args=list(...))
}

#' @export
KmknnParam_kmeans_args <- function(x) {
    x@kmeans.args
}

##################################
###### KmknnIndex methods ########
##################################

#' @export
#' @importFrom methods new
KmknnIndex <- function(data, centers, info, order, NAMES=NULL) {
    new("KmknnIndex", data=data, centers=centers, info=info, order=order, NAMES=NAMES)
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


#' @export
setMethod("show", "KmknnIndex", function(object) {
    callNextMethod()
    cat(sprintf("clusters: %i\n", ncol(KmknnIndex_cluster_centers(object))))
})

        
KmknnIndex_cluster_centers <- function(x) {
    x@centers
}

#' @export
KmknnIndex_cluster_info <- function(x) {
    x@info
}

#' @export
setMethod("bnorder", "KmknnIndex", function(x) x@order)
