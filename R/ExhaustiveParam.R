#' The ExhaustiveParam class
#'
#' A class to hold parameters for the exhaustive algorithm for exact nearest neighbor identification.
#' 
#' @param distance String specifying the distance metric to use.
#' Cosine distances are implemented as Euclidean distances on L2-normalized coordinates.
#' @param BNPARAM An ExhaustiveParam instance.
#' 
#' @details
#' The exhaustive search computes all pairwise distances between data and query points to identify nearest neighbors of the latter.
#' It has quadratic complexity and is theoretically the worst-performing method;
#' however, it has effectively no overhead from constructing or querying indexing structures, 
#' making it faster for in situations where indexing provides little benefit.
#' This includes queries against datasets with few data points or very high dimensionality.
#'
#' All that said, this algorithm is largely provided as a baseline for comparing against the other algorithms.
#'
#' @return
#' The \code{ExhaustiveParam} constructor returns an instance of the ExhaustiveParam class.
#'
#' The \code{\link{defineBuilder}} method returns an external pointer that can be used in \code{\link{buildIndex}} to construct an exhaustive index.
#' 
#' @author
#' Allison Vuong
#' 
#' @seealso
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- ExhaustiveParam())
#'
#' @aliases
#' ExhaustiveParam-class
#' ExhaustiveIndex
#' ExhaustiveIndex-class
#' 
#' @docType class
#' 
#' @export
#' @importFrom methods new
ExhaustiveParam <- function(distance=c("Euclidean", "Manhattan", "Cosine")) {
    new("ExhaustiveParam", distance=match.arg(distance))
}

#' @export
ExhaustiveIndex <- function(ptr, names) {
    new("ExhaustiveIndex", ptr=ptr, names=names)
}

#' @export
#' @rdname ExhaustiveParam
setMethod("defineBuilder", "ExhaustiveParam", function(BNPARAM) {
    list(
        builder=exhaustive_builder(distance=BNPARAM@distance),
        class=ExhaustiveIndex
    )
})
