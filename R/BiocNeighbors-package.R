#' Nearest Neighbor Detection for Bioconductor Packages
#'
#' Implements exact and approximate methods for nearest neighbor detection, in a framework that allows them to be easily switched within Bioconductor packages or workflows.
#' Exact searches can be performed using the k-means for k-nearest neighbors algorithm, vantage point trees, or an exhaustive search.
#' Approximate searches can be performed using the Annoy or HNSW libraries.
#' Each search can be performed with a variety of different distance metrics, parallelization, and variable numbers of neighbors.
#' Range-based searches (to find all neighbors within a certain distance) are also supported.
#'
#' @import methods
#' @importFrom Rcpp sourceCpp
#' @useDynLib BiocNeighbors
"_PACKAGE"
