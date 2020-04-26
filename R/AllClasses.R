# Defines the BiocNeighborParam class and derivatives.

#' @export
setClass("BiocNeighborParam", contains="VIRTUAL", slots=c(distance="character"))

#' @exportClass
setClass("ExhaustiveParam", contains="BiocNeighborParam")

#' @export
setClass("KmknnParam", contains="BiocNeighborParam", slots=c(kmeans.args="list"))

#' @export
setClass("VptreeParam", contains="BiocNeighborParam")

#' @export
setClass("AnnoyParam", contains="BiocNeighborParam", slots=c(ntrees="integer", dir="character", search.mult="numeric")) 

#' @export
setClass("HnswParam", contains="BiocNeighborParam", slots=c(nlinks="integer", ef.construction="integer", dir="character", ef.search="integer")) 

# Defines the BiocNeighborIndex class and derivatives.

#' @export
#' @importClassesFrom S4Vectors character_OR_NULL
setClass("BiocNeighborIndex", contains="VIRTUAL", slots=c(data="matrix", NAMES="character_OR_NULL", distance="character"))

#' @exportClass
setClass("ExhaustiveIndex", contains="BiocNeighborIndex")

#' @export
setClass("KmknnIndex", contains="BiocNeighborIndex", slots=c(centers="matrix", info="list", order="integer"))

#' @export
setClass("VptreeIndex", contains="BiocNeighborIndex", slots=c(order="integer", nodes="list"))

#' @export
setClass("AnnoyIndex", contains="BiocNeighborIndex", slots=c(path="character", search.mult="numeric"))

#' @export
setClass("HnswIndex", contains="BiocNeighborIndex", slots=c(path="character", ef.search="integer"))
