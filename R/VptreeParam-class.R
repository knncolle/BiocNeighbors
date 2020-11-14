#' The VptreeParam class
#'
#' A class to hold parameters for the VP tree algorithm for exact nearest neighbor identification.
#' 
#' @param distance A string specifying the distance metric to use.
#' 
#' @return  
#' An instance of the VptreeParam class.
#' 
#' @details
#' Users can get or set values with the usual \code{[[} syntax.
#' All parameters listed in the constructor can be manipulated in this manner.
#' 
#' @author
#' Aaron Lun
#' 
#' @seealso
#' \code{\link{buildVptree}}, for the index construction.
#'
#' \code{\link{findVptree}} and related functions, for the actual search. 
#'
#' \linkS4class{BiocNeighborParam}, for the parent class and its available methods.
#' 
#' @examples
#' (out <- VptreeParam())
#'
#' @aliases
#' VptreeParam-class
#'
#' @export
#' @importFrom methods new
VptreeParam <- function(distance="Euclidean") {
    new("VptreeParam", distance=distance)
}

setMethod("spill_args", "VptreeParam", function(x) {
    list(distance=bndistance(x))
})

