\name{Search algorithms}
\alias{BiocNeighbors-algorithms}

\title{Neighbor search algorithms}
\description{This page provides an overview of the neighbor search algorithms available in \pkg{BiocNeighbors}.}

\section{K-means with k-nearest neighbors (KMKNN)}{
The KMKNN algorithm was proposed by Wang (2012) to quickly identify k-nearest neighbors in high-dimensional data.
Briefly, data points are rapidly clustered into \code{N} clusters using k-means clustering, where \code{N} is the square root of the number of points.
This clustering is then used to speed up the nearest neighbor search across the input data set,
exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.
}

\section{Vantage point (VP) trees}{
VP trees (Yianilos, 1993) are designed to quickly identify k-nearest neighbors in high-dimensional data.
In the VP tree, each node contains a subset of points and has a defined threshold distance (usually the median distance to all points in the subset).
The left child contains the further subset of points within the radius, while the right child contains the remaining points in the subset that are outside.
The nearest neighbor search across \code{X} exploits the triangle inequality between node centers and thresholds to narrow the search space.
} 

\section{Approximate nearest neighbors Oh Yeah (Annoy)}{ 
The Annoy method was developed by Erik Bernhardsson to identify approximate k-nearest neighbors in high-dimensional data.
Briefly, a tree is constructed by using a random hyperplane to split the points at each internal node.
For a given input data point, all points in the same leaf node are defined as a set of potential nearest neighbors.
This is repeated across multiple trees, and the union of all such sets is searched to identify the actual nearest neighbors.
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


