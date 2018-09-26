# Getter methods for KmknnParam.

#' @export
KmknnParam_kmeans_args <- function(x) {
    x@kmeans.args
}

# Getter methods for AnnoyParam.

#' @export
AnnoyParam_ntrees <- function(x) {
    x@ntrees
}

#' @export
AnnoyParam_directory <- function(x) {
    x@dir
}

# Getter methods for BiocNeighborIndex

#' @export
setMethod("dimnames", "BiocNeighborIndex", function(x) {
    list(x@NAMES, NULL) 
})

# Getter methods for KmknnIndex

#' @export
KmknnIndex_clustered_data <- function(x) {
    x@data
}

#' @export
KmknnIndex_cluster_centers <- function(x) {
    x@centers
}

#' @export
KmknnIndex_cluster_info <- function(x) {
    x@info
}

#' @export
KmknnIndex_clustered_order <- function(x) {
    x@order
}

setMethod("dim", "KmknnIndex", function(x) { 
    rev(dim(KmknnIndex_clustered_data(x))) # reversed, as matrix was transposed.
}) 

# Getter methods for AnnoyIndex

#' @export
AnnoyIndex_path <- function(x) {
    x@path
}

#' @export
setMethod("dim", "AnnoyIndex", function(x) { x@Dims })
