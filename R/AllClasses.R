# Defines the BiocNeighborParam class and derivatives.

#' @export
setClass("BiocNeighborParam", contains="VIRTUAL", slots=c(distance="character"))

#' @export
setClass("ExhaustiveParam", contains="BiocNeighborParam")

#' @export
setClass("KmknnParam", contains="BiocNeighborParam")

#' @export
setClass("VptreeParam", contains="BiocNeighborParam")

#' @export
setClass("AnnoyParam", contains="BiocNeighborParam", slots=c(ntrees="integer", search.mult="numeric")) 

#' @export
setClass("HnswParam", contains="BiocNeighborParam", slots=c(nlinks="integer", ef.construction="integer", ef.search="integer")) 
