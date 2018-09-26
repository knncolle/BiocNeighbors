#' @export
#' @importFrom methods new
KmknnParam <- function() {
    new("KmknnParam")
}

#' @export
#' @importFrom methods new
KmknnIndex <- function(data, centers, info, order) {
    new("KmknnIndex", data=data, centers=centers, info=info, order=order)
}

#' @importFrom S4Vectors setValidity2
setValidity2("KmknnIndex", function(object) {
    msg <- character(0)

    data <- KmknnIndex_clustered_data(object)
    centers <- KmknnIndex_cluster_centers(object)
    if (nrow(data)!=nrow(centers)) {
        msg <- c(msg, "dimensionality is not consistent between 'data' and 'centers") 
    }

    info <- KmknnIndex_cluster_info(object)
    if (length(info)!=ncol(centers)) {
        msg <- c(msg, "number of clusters is not consistent between 'centers' and 'info'")
    }

    order <- KmknnIndex_clustered_order(object)
    if (length(order)!=ncol(data)) {
        msg <- c(msg, "number of observations is not consistent between 'data' and 'order'")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})
