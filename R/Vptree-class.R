#' @export
#' @importFrom methods new
VptreeParam <- function(distance="Euclidean") {
    new("VptreeParam", distance=distance)
}

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
VptreeIndex_nodes <- function(x) {
    x@nodes
}

