\name{Search algorithms}
\alias{BiocNeighbors-algorithms}

\title{Neighbor search algorithms}
\description{This page provides an overview of the neighbor search algorithms available in \pkg{BiocNeighbors}.}

\section{K-means with k-nearest neighbors (KMKNN)}{
In the KMKNN algorithm (Wang, 2012), k-means clustering is first applied to the data points using the square root of the number of points as the number of cluster centers.
The cluster assignment and distance to the assigned cluster center for each point represent the KMKNN indexing information. 
This speeds up the nearest neighbor search by exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.
The advantage of the KMKNN approach is its simplicity and minimal overhead,
resulting in performance improvements over conventional tree-based methods for high-dimensional data where most points need to be searched anyway.
It is also trivially extended to find all neighbors within a threshold distance from a query point.
}

\section{Vantage point (VP) trees}{
In a VP tree (Yianilos, 1993), each node contains a subset of points and has a defined threshold distance (usually the median distance to all points in the subset).
The left child of this node contains the further subset of points within the radius, while the right child contains the remaining points in the subset that are outside.
The nearest neighbor search traverses the tree and exploits the triangle inequality between query points, node centers and thresholds to narrow the search space.
VP trees are often faster than more conventional KD-trees or ball trees as the former uses the points themselves as the nodes of the tree,
avoiding the need to create many intermediate nodes and reducing the total number of distance calculations.
Like KMKNN, it is also trivially extended to find all neighbors within a threshold distance from a query point.
} 

\section{Approximate nearest neighbors Oh Yeah (Annoy)}{ 
The Annoy algorithm was developed by Erik Bernhardsson to identify approximate k-nearest neighbors in high-dimensional data.
Briefly, a tree is constructed where a random hyperplane splits the points into two subsets at each internal node.
Leaf nodes are defined when the number of points in a subset falls below a threshold (close to twice the number of dimensions for the settings used here).
Multiple trees are constructed in this manner, each of which is different due to the random choice of hyperplanes.
For a given query point, each tree is searched to identify the subset of all points in the same leaf node as the query point. 
The union of these subsets across all trees is exhaustively searched to identify the actual nearest neighbors to the query.
}

\section{Distance metrics}{ 
Currently, only Euclidean distances are supported, but support may be added for other distance types depending on demand.
}

\author{
Aaron Lun, using code from the \pkg{cydar} package (Lun et al., 2017) for the KMKNN implementation;
from Steve Hanov, for the VP tree implementation;
and \pkg{RcppAnnoy}, for the Annoy implementation. 
}

\references{
Wang X (2012). 
A fast exact k-nearest neighbors algorithm for high dimensional search using k-means clustering and triangle inequality. 
\emph{Proc Int Jt Conf Neural Netw}, 43, 6:2351-2358.

Lun ATL, Richard AC, Marioni JC (2017). 
Testing for differential abundance in mass cytometry data. 
\emph{Nat. Methods}, 14, 7:707-709.

Hanov S (2011).
VP trees: A data structure for finding stuff fast.
\url{http://stevehanov.ca/blog/index.php?id=130}

Yianilos PN (1993).
Data structures and algorithms for nearest neighbor search in general metric spaces.
\emph{Proceedings of the Fourth Annual ACM-SIAM Symposium on Discrete Algorithms}, 311-321 

Bernhardsson E (2018).
Annoy.
\url{https://github.com/spotify/annoy}
}


