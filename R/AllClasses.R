# Defines the BiocNeighborParam class and derivatives.

setClass("BiocNeighborParam", contains="VIRTUAL")

setClass("KmknnParam", contains="BiocNeighborParam", slots=c(precomputed="list", raw.index="logical"))

setClass("AnnoyParam", contains="BiocNeighborParam", slots=c(ntrees="integer", index.dir="character", index.path="character"))
