#' The VptreeIndex class
#'
#' A class to hold the vantage point tree for exact nearest neighbor identification.
#' 
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param nodes A list of vectors specifying the structure of the VP tree.
#' @param order An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#' @param x A VptreeIndex object.
#' 
#' @details
#' The VptreeIndex class holds the indexing structure required to run the VP tree algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of VptreeIndex classes with \code{\link{buildVptree}}.
#' 
#' Users can get values from a VptreeIndex object with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be extracted in this manner.
#' 
#' @return
#' An instance of the VptreeIndex class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildVptree}}, for the index construction.
#'
#' \linkS4class{BiocNeighborIndex}, for the parent class and its available methods.
#' 
#' @examples
#' example(buildVptree)
#' str(VptreeIndex_nodes(out))
#'
#' @docType class
#' @aliases
#' VptreeIndex-class
#' bnorder,VptreeIndex-method
#'
#' VptreeIndex_nodes
#'
#' @export
#' @importFrom methods new
VptreeIndex <- function(data, nodes, order, NAMES=NULL, distance="Euclidean") {
    new("VptreeIndex", data=data, nodes=nodes, order=order, NAMES=NAMES, distance=distance)
}

#' @importFrom S4Vectors setValidity2
setValidity2("VptreeIndex", function(object) {
    msg <- character(0)

    data <- bndata(object)
    order <- bnorder(object)
    if (length(order)!=ncol(data)) {
        msg <- c(msg, "number of observations is not consistent between 'data' and 'order'")
    }

    node.len <- lengths(object[['nodes']])
    if (length(node.len)!=4) {
        msg <- c(msg, "node information should contain 4 vectors")
    }
    if (length(unique(node.len))!=1) {
        msg <- c(msg, "node information vectors should have same length")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})

#' @export
VptreeIndex_nodes <- function(x) {
    .Deprecated(new="x[['nodes']]")
    x@nodes
}

#' @export
setMethod("bnorder", "VptreeIndex", function(x) x@order)

.find_vptree_args <- function(precomputed) {
    list(
        nodes=precomputed[['nodes']]
    )
}
