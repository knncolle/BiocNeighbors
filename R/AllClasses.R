# Defines the BiocNeighborParam class and derivatives.

#' @export
setClass("BiocNeighborParam", contains="VIRTUAL")

#' @export
setClass("KmknnParam", contains="BiocNeighborParam")

#' @export
setClass("AnnoyParam", contains="BiocNeighborParam", slots=c(ntrees="integer", dir="character")) 

# Defines the BiocNeighborIndex class and derivatives.

#' @export
setClass("BiocNeighborIndex", contains="VIRTUAL")

#' @export
setClass("KmknnIndex", contains="BiocNeighborIndex", slots=c(data="matrix", centers="matrix", info="list", order="integer"))

#' @export
setClass("AnnoyIndex", contains="BiocNeighborIndex", slots=c(path="character", Dims="integer"))
