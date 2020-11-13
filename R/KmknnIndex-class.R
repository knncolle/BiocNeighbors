#' The KmknnIndex class
#'
#' A class to hold indexing structures for the KMKNN algorithm for exact nearest neighbor identification.
#' 
#' @param data A numeric matrix where columns correspond to data points and rows correspond to dimensions.
#' @param centers A numeric matrix containing coordinates for cluster centroids, with clusters in columns and dimensions in rows.
#' @param info A list containing additional information for each cluster, see \code{\link{buildKmknn}} for details.
#' @param order An integer vector of length equal to \code{ncol(data)}, specifying the order of points in \code{x} relative to the original data matrix.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#' 
#' @details
#' The KmknnIndex class holds the indexing structure required to run the KMKNN algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of KmknnIndex classes with \code{\link{buildKmknn}}.
#' 
#' Users can get values from an HnswIndex object with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be extracted in this manner.
#' 
#' @return An instance of the KmknnIndex class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildKmknn}}, to build the index.
#' 
#' \linkS4class{BiocNeighborIndex}, for the parent class and its available methods.
#' 
#' @examples
#' example(buildKmknn)
#' out[['centers']]
#' out[['info']]
#'
#' @aliases
#' KmknnIndex-class
#' KmknnIndex_cluster_centers
#' KmknnIndex_cluster_info
#' show,KmknnIndex-method
#' bnorder,KmknnIndex-method
#' @docType class
#' 
#' @export
#' @importFrom methods new
KmknnIndex <- function(data, centers, info, order, NAMES=NULL, distance="Euclidean") {
    new("KmknnIndex", data=data, centers=centers, info=info, order=order, NAMES=NAMES, distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("KmknnIndex", function(object) {
    msg <- character(0)

    data <- bndata(object)
    centers <- object[['centers']]
    if (nrow(data)!=nrow(centers)) {
        msg <- c(msg, "dimensionality is not consistent between 'data' and 'centers") 
    }

    info <- object[['info']]
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
    cat(sprintf("clusters: %i\n", ncol(object[['centers']])))
})

#' @export        
KmknnIndex_cluster_centers <- function(x) {
    .Deprecated(new="x[['centers']]")
    x@centers
}

#' @export
KmknnIndex_cluster_info <- function(x) {
    .Deprecated(new="x[['info']]")
    x@info
}

#' @export
setMethod("bnorder", "KmknnIndex", function(x) x@order)

.find_kmknn_args <- function(precomputed) {
    list(
        clust_centers=precomputed[['centers']],
        clust_info=precomputed[['info']]
    )
}
