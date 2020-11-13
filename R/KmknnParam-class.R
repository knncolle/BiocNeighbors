#' The KmknnParam class
#'
#' A class to hold parameters for the KMKNN algorithm for exact nearest neighbor identification.
#'
#' @param ... Arguments to be passed to \code{\link{kmeans}}.
#' @param distance A string specifying the distance metric to use.
#'
#' @details
#' The KmknnParam class holds any parameters associated with running the KMKNN algorithm.
#' Currently, this relates to tuning of the k-means step - see \code{\link{buildKmknn}} for details.
#' 
#' Users can get or set values from an KmknnParam object with the usual \code{[[} syntax.
#' All parameters listed in \code{...} are available via \code{x[['kmeans.args']]}.
#'
#' @return
#' An instance of the KmknnParam class.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildKmknn}}, for the index construction.
#'
#' \code{\link{findKmknn}} and related functions, for the actual search. 
#'
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- KmknnParam(iter.max=100))
#' out[['kmeans.args']]
#'
#' @docType class
#' @aliases
#' KmknnParam-class
#' show,KmknnParam-method
#' KmknnParam_kmeans_args
#'
#' @export
#' @importFrom methods new
KmknnParam <- function(..., distance="Euclidean") {
    new("KmknnParam", kmeans.args=list(...), distance=distance)
}

#' @export
KmknnParam_kmeans_args <- function(x) {
    .Deprecated(new="x[['kmeans.args']]")
    x@kmeans.args
}

#' @export
setMethod("show", "KmknnParam", function(object) {
    callNextMethod()

    all.args <- names(object[['kmeans.args']])
    all.args[is.na(all.args)] <- ""
    N <- length(all.args)
    if (N >= 4L) all.args <- c(all.args[seq_len(3)], "...")

    cat(sprintf("kmeans args(%i): %s\n", N, paste(all.args, collapse=" ")))
})

setMethod("spill_args", "KmknnParam", function(x) {
    c(list(distance=bndistance(x)), x[['kmeans.args']])
})
