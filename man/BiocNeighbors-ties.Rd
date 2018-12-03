\name{Tied distances}
\alias{BiocNeighbors-ties}

\title{Handling tied distances}
\description{Interpreting the warnings when distances are tied in an exact nearest neighbor (NN) search.}

\section{The problem of ties}{
A warning will be raised if ties are detected among the \code{k+1} NNs for any of the exact NN search methods. 
Specifically, ties are detected when a larger distance is less than \code{(1 + 1e-8)}-fold of the smaller distance.
This criterion tends to be somewhat conservative in the sense that it will warn users even if there is no problem (i.e., the distances are truly different).
However, more accurate detection is difficult to achieve due to the vagaries of numerical precision across different machines.

The most obvious problem with ties is that it may affect the identity of the reported neighbors.
The various NN search functions will return a constant number of neighbors for each data point.
If the \code{k}th neighbor is tied with the \code{k+1}th neighbor, this requires an arbitrary decision about which data point to retain in the NN set.
A milder issue is that the order of the neighbors within the set is arbitrary, which may be important for certain algorithms.
}

\section{Interaction with random seeds}{
In general, the NN search algorithms in this package are fully deterministic, despite the use of stochastic steps during index construction.
The only exception occurs when there are tied distances to neighbors, at which point the order and/or identity of the k-nearest neighboring points is not well-defined.
This is because, In the presence of ties, the output will depend on the ordering of points in the \code{\link{buildKmknn}} output.

Users should set the seed to guarantee consistent (albeit arbitrary) results across different runs of the function.
Note, however, that the exact selection of tied points depends on the numerical precision of the system.
Thus, even after setting a seed, there is no guarantee that the results will be reproducible across machines (especially Windows)!
}

\seealso{
\code{\link{findKmknn}} and \code{\link{findVptree}} for examples where tie warnings are produced.
}

\author{
Aaron Lun
}

\examples{
vals <- matrix(0, nrow=10, ncol=20)
out <- findKmknn(vals, k=5)
}
