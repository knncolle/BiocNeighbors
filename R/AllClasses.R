# Defines the BiocNeighborParam class and derivatives.

setClass("BiocNeighborParam", contains="VIRTUAL")

setClass("KmknnParam", contains="BiocNeighborParam")

setClass("AnnoyParam", contains="BiocNeighborParam", slots=c(ntrees="integer", dir="character")) 

# Defines the BiocNeighborIndex class and derivatives.

setClass("BiocNeighborIndex", contains="VIRTUAL")

setClass("KmknnIndex", contains="BiocNeighborIndex", slots=c(data="matrix", centers="matrix", info="list", order="integer"))

setClass("AnnoyIndex", contains="BiocNeighborIndex", slots=c(path="character", Dims="integer"))
