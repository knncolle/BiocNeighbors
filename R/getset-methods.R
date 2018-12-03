##################################
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

##################################
# Getter methods for BiocNeighborIndex

#' @export
setMethod("dimnames", "BiocNeighborIndex", function(x) {
    list(x@NAMES, NULL) 
})

#' @export
setMethod("bndata", "BiocNeighborIndex", function(x) x@data)

#' @export
setMethod("dim", "BiocNeighborIndex", function(x) rev(dim(bndata(x))) ) # reversed, as matrix was transposed.

# Getter methods for KmknnIndex

#' @export
KmknnIndex_cluster_centers <- function(x) {
    x@centers
}

#' @export
KmknnIndex_cluster_info <- function(x) {
    x@info
}

#' @export
setMethod("bnorder", "KmknnIndex", function(x) x@order)

# Getter methods for VptreeIndex

#' @export
VptreeIndex_nodes <- function(x) {
    x@nodes
}

#' @export
setMethod("bnorder", "VptreeIndex", function(x) x@order)

# Getter methods for AnnoyIndex

#' @export
AnnoyIndex_path <- function(x) {
    x@path
}

setMethod("bnorder", "AnnoyIndex", function(x) seq_len(ncol(bndata(x))) )
