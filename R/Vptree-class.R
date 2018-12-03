#' @export
#' @importFrom methods new
VptreeParam <- function() {
    new("VptreeParam")
}

#' @export
#' @importFrom methods new
VptreeIndex <- function(data, nodes, order, NAMES=NULL) {
    new("VptreeIndex", data=data, nodes=nodes, order=order, NAMES=NAMES)
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

    NAMES <- rownames(object)
    if (!is.null(NAMES) && length(NAMES)!=nrow(object)) {
        msg <- c(msg, "length of non-NULL 'NAMES' is not equal to the number of rows")
    }

    if (length(msg)) return(msg)
    return(TRUE)
})
