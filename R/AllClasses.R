# Defines the BiocNeighborParam class and derivatives.

#' @export
setClass("BiocNeighborParam", contains="VIRTUAL")

#' @export
setClass("KmknnParam", contains="BiocNeighborParam", slots=c(kmeans.args="list"))

#' @export
setClass("AnnoyParam", contains="BiocNeighborParam", slots=c(ntrees="integer", dir="character")) 

# Defines the BiocNeighborIndex class and derivatives.

#' @export
#' @importClassesFrom S4Vectors character_OR_NULL
setClass("BiocNeighborIndex", contains="VIRTUAL", slots=c(NAMES="character_OR_NULL"))

#' @export
setClass("KmknnIndex", contains="BiocNeighborIndex", slots=c(data="matrix", centers="matrix", info="list", order="integer"))

#' @export
setClass("AnnoyIndex", contains="BiocNeighborIndex", slots=c(path="character", Dims="integer"))
