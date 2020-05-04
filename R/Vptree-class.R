##################################
###### VptreeParam methods #######
##################################
#' The VptreeParam class
#'
#' A class to hold parameters for the VP tree algorithm for exact nearest neighbor identification.
#'
#' @param distance A string specifying the distance metric to use.
#' Defaults to \code{"Euclidean"}.
#'
#' @return A VptreeParam object.
#'
#' @examples
#' out <- VptreeParam()
#'
#' @seealso 
#' \code{\link{buildVptree}}. 
#' @author Aaron Lun
#'
#' @export
#' @importFrom methods new
VptreeParam <- function(distance="Euclidean") {
    new("VptreeParam", distance=distance)
}

setMethod("spill_args", "VptreeParam", function(x) {
    list(distance=bndistance(x))
})

##################################
###### VptreeIndex methods #######
##################################
#' The VptreeIndex class
#'
#' A class to hold the vantage point tree for exact nearest neighbor identification.
#'
#' @param data A numeric matrix with data points in columns and dimensions in rows.
#' @param nodes A list of vectors specifying the structure of the VP tree.
#' @param order An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.
#' @param NAMES A character vector of sample names or \code{NULL}.
#' @param distance A string specifying the distance metric to use.
#'
#' @return A VptreeIndex object.
#'
#' @details
#' The VptreeIndex class holds the indexing structure required to run the Vptree algorithm.
#' Users should never need to call the constructor explicitly, but should generate instances of VptreeIndex classes with \code{\link{buildVptree}}.
#'
#' @examples
#' out <- example(buildVptree)
#' str(VptreeIndex_nodes(out))
#'
#' @seealso 
#' \code{\link{buildVptree}}. 
#' @author Aaron Lun

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

    node.len <- lengths(VptreeIndex_nodes(object))
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
#' @rdname VptreeIndex
VptreeIndex_nodes <- function(x) {
    x@nodes
}

#' @exportMethod bnorder
#' @rdname VptreeIndex
#' @aliases VptreeIndex-method
setMethod("bnorder", "VptreeIndex", function(x) x@order)
