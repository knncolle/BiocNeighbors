#' @import BiocParallel
#' @import methods
#' @importFrom Rcpp sourceCpp
#' @useDynLib BiocNeighbors
#' @name BiocNeighbors-package
"_PACKAGE"

#' Neighbor search algorithms
#'
#' This page provides an overview of the neighbor search algorithms available in \pkg{BiocNeighbors}.
#' 
#' @section K-means with k-nearest neighbors (KMKNN):
#' In the KMKNN algorithm (Wang, 2012), k-means clustering is first applied to the data points using the square root of the number of points as the number of cluster centers.
#' The cluster assignment and distance to the assigned cluster center for each point represent the KMKNN indexing information. 
#' This speeds up the nearest neighbor search by exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.
#' The advantage of the KMKNN approach is its simplicity and minimal overhead,
#' resulting in performance improvements over conventional tree-based methods for high-dimensional data where most points need to be searched anyway.
#' It is also trivially extended to find all neighbors within a threshold distance from a query point.
#' 
#' @section Vantage point (VP) trees:
#' In a VP tree (Yianilos, 1993), each node contains a subset of points that is split into two further partitions.
#' The split is determined by picking an arbitrary point inside that subset as the node center, 
#' computing the distance to all other points from the center, and taking the median as the \dQuote{radius}.
#' The left child of this node contains all points within the median distance from the radius, while the right child contains the remaining points.
#' This is applied recursively until all points resolve to individual nodes.
#' The nearest neighbor search traverses the tree and exploits the triangle inequality between query points, node centers and thresholds to narrow the search space.
#' VP trees are often faster than more conventional KD-trees or ball trees as the former uses the points themselves as the nodes of the tree,
#' avoiding the need to create many intermediate nodes and reducing the total number of distance calculations.
#' Like KMKNN, it is also trivially extended to find all neighbors within a threshold distance from a query point.
#' 
#' @section Exhaustive search: 
#' The exhaustive search computes all pairwise distances between data and query points to identify nearest neighbors of the latter.
#' It has quadratic complexity and is theoretically the worst-performing method;
#' however, it has effectively no overhead from constructing or querying indexing structures, 
#' making it faster for in situations where indexing provides little benefit.
#' This includes queries against datasets with few data points or very high dimensionality.
#' 
#' @section Approximate nearest neighbors Oh Yeah (Annoy): 
#' The Annoy algorithm was developed by Erik Bernhardsson to identify approximate k-nearest neighbors in high-dimensional data.
#' Briefly, a tree is constructed where a random hyperplane splits the points into two subsets at each internal node.
#' Leaf nodes are defined when the number of points in a subset falls below a threshold (close to twice the number of dimensions for the settings used here).
#' Multiple trees are constructed in this manner, each of which is different due to the random choice of hyperplanes.
#' For a given query point, each tree is searched to identify the subset of all points in the same leaf node as the query point. 
#' The union of these subsets across all trees is exhaustively searched to identify the actual nearest neighbors to the query.
#' 
#' @section Hierarchical navigable small worlds (HNSW):
#' In the HNSW algorithm (Malkov and Yashunin, 2016), each point is a node in a \dQuote{nagivable small world} graph.
#' The nearest neighbor search proceeds by starting at a node and walking through the graph to obtain closer neighbors to a given query point.
#' Nagivable small world graphs are used to maintain connectivity across the data set by creating links between distant points.
#' This speeds up the search by ensuring that the algorithm does not need to take many small steps to move from one cluster to another.
#' The HNSW algorithm extends this idea by using a hierarchy of such graphs containing links of different lengths, 
#' which avoids wasting time on small steps in the early stages of the search where the current node position is far from the query.
#' 
#' @section Distance metrics: 
#' All algorithms support neighbor searching by Euclidean, Manhattan and cosine distances.
#' Cosine distances are implemented as the Euclidean distance between L2-normalized vectors.
#' Note that KMKNN operates much more naturally with Euclidean distances, so your mileage may vary when using it with Manhattan distances.
#' 
#' @author
#' Aaron Lun, using code from the \pkg{cydar} package for the KMKNN implementation;
#' from Steve Hanov, for the VP tree implementation;
#' \pkg{RcppAnnoy}, for the Annoy implementation;
#' and \pkg{RcppHNSW}, for the HNSW implementation.
#' 
#' @references
#' Wang X (2012). 
#' A fast exact k-nearest neighbors algorithm for high dimensional search using k-means clustering and triangle inequality. 
#' \emph{Proc Int Jt Conf Neural Netw}, 43, 6:2351-2358.
#' 
#' Hanov S (2011).
#' VP trees: A data structure for finding stuff fast.
#' \url{http://stevehanov.ca/blog/index.php?id=130}
#' 
#' Yianilos PN (1993).
#' Data structures and algorithms for nearest neighbor search in general metric spaces.
#' \emph{Proceedings of the Fourth Annual ACM-SIAM Symposium on Discrete Algorithms}, 311-321.
#' 
#' Bernhardsson E (2018).
#' Annoy.
#' \url{https://github.com/spotify/annoy}
#' 
#' Malkov YA, Yashunin DA (2016).
#' Efficient and robust approximate nearest neighbor search using Hierarchical Navigable Small World graphs.
#' \emph{arXiv}.
#' \url{https://arxiv.org/abs/1603.09320}
#'
#' @name BiocNeighbors-algorithms
NULL

#' Reporting raw indices
#'
#' An overview of what raw indices mean for neighbor-search implementations that contain a rearranged matrix in the \linkS4class{BiocNeighborIndex} object.
#' 
#' @section What are raw indices?:
#' Consider the following call:
#' \preformatted{    index <- buildKmknn(vals)    
#'     out <- findKmknn(precomputed=index, k=k, raw.index=TRUE)
#' }
#' This yields the same output as:
#' \preformatted{    PRE <- bndata(index)
#'     out2 <- findKmknn(X=t(PRE), k=k)
#' }
#' 
#' When \code{raw.index=TRUE} in the first call, the indices in \code{out$index} matrix can be imagined to refer to \emph{columns} of \code{PRE} in the second call.
#' Moreover, all function arguments that previously referred to rows of \code{X} (e.g., \code{subset}) are now considered to refer to columns of \code{PRE}.
#' 
#' The same reasoning applies to all functions where \code{precomputed} can be specified in place of \code{X}.
#' This includes query-based searches (e.g., \code{\link{queryKmknn}}) and range searches (\code{\link{rangeFindKmknn}}).
#' 
#' @section Motivation: 
#' Setting \code{raw.index=TRUE} is intended for scenarios where the reordered data in \code{precomputed} is used elsewhere.
#' By returning indices to the reordered data, the user does not need to hold onto the original data and/or switch between the original ordering and that in \code{precomputed}.
#' This simplifies downstream code and provides a slight speed boost by avoiding the need for re-indexing.
#' 
#' Neighbor search implementations can only return raw indices if their index construction involves transposing \code{X} and reordering its columns.
#' This tends to be the case for most implementations as transposition allows efficient column-major distance calculations and reordering improves data locality.
#' Both the KMKNN and VP tree implementations fulfill these requirements and thus have the \code{raw.index} option.
#' 
#' Note that setting \code{raw.index=TRUE} makes little sense when \code{precomputed} is not specified.
#' When \code{precomputed=NULL}, a temporary index will be constructed that is not visible in the calling scope.
#' As index construction may be stochastic, the raw indices will not refer to anything that is meaningful to the end-user.
#' 
#' @seealso
#' \code{\link{findKmknn}} and \code{\link{findVptree}} for examples where raw indices are used.
#' 
#' @author
#' Aaron Lun
#' 
#' @examples
#' vals <- matrix(rnorm(100000), ncol=20)
#' index <- buildKmknn(vals)    
#' out <- findKmknn(precomputed=index, raw.index=TRUE, k=5)
#' alt <- findKmknn(t(bndata(index)), k=5)    
#' head(out$index)
#' head(alt$index)
#'
#' @name BiocNeighbors-raw-index 
NULL

#' Handling tied distances
#'
#' Interpreting the warnings when distances are tied in an exact nearest neighbor (NN) search.
#' 
#' @section The problem of ties:
#' A warning will be raised if ties are detected among the \code{k+1} NNs for any of the exact NN search methods. 
#' Specifically, ties are detected when a larger distance is less than \code{(1 + 1e-8)}-fold of the smaller distance.
#' This criterion tends to be somewhat conservative in the sense that it will warn users even if there is no problem (i.e., the distances are truly different).
#' However, more accurate detection is difficult to achieve due to the vagaries of numerical precision across different machines.
#' 
#' The most obvious problem with ties is that it may affect the identity of the reported neighbors.
#' The various NN search functions will return a constant number of neighbors for each data point.
#' If the \code{k}th neighbor is tied with the \code{k+1}th neighbor, this requires an arbitrary decision about which data point to retain in the NN set.
#' A milder issue is that the order of the neighbors within the set is arbitrary, which may be important for certain algorithms.
#' 
#' @section Interaction with random seeds:
#' In general, the exact NN search algorithms in this package are fully deterministic despite the use of stochastic steps during index construction.
#' The only exception occurs when there are tied distances to neighbors, at which point the order and/or identity of the k-nearest neighboring points is not well-defined.
#' This is because, In the presence of ties, the output will depend on the ordering of points in the constructed index from \code{\link{buildKmknn}} or \code{\link{buildVptree}}.
#' 
#' Users should set the seed to guarantee consistent (albeit arbitrary) results across different runs of the function.
#' However, note that the exact selection of tied points depends on the numerical precision of the system.
#' Thus, even after setting a seed, there is no guarantee that the results will be reproducible across machines (especially Windows)!
#'
#' @section Turning off the warnings:
#' It may ocassionally be appropriate to disable the warnings by setting \code{warn.ties=FALSE}.
#' The most obvious scenario is when \code{get.index=FALSE}, i.e., we are only interested in the distances to the neighbors.
#' In such cases, the presence of ties does not matter as changes to the identity of tied neighbors do not affect the returned distances (which, for ties, are equal by definition).
#' Similarly, if the seed is set prior to the search, the warnings are unnecessary as the output is fully deterministic.
#' 
#' @seealso
#' \code{\link{findKmknn}} and \code{\link{findVptree}} for examples where tie warnings are produced.
#' 
#' @author
#' Aaron Lun
#' 
#' @examples
#' vals <- matrix(0, nrow=10, ncol=20)
#' out <- findKmknn(vals, k=5)
#'
#' @name BiocNeighbors-ties
NULL
