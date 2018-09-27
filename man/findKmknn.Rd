\name{findKmknn}
\alias{findKmknn}

\title{Find nearest neighbors}
\description{Use the KMKNN (K-means for k-nearest neighbors) algorithm to identify nearest neighbors from a dataset.}

\usage{
findKmknn(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, raw.index=FALSE, ...)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{BPPARAM}{A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.}
\item{precomputed}{A \linkS4class{KmknnIndex} object returned from running \code{\link{buildKmknn}} on \code{X}.}
\item{subset}{A vector indicating the rows of \code{X} for which the nearest neighbors should be identified.}
\item{raw.index}{A logical scalar indicating whether column indices to the reordered data in \code{precomputed} should be directly returned.}
\item{...}{Further arguments to pass to \code{\link{buildKmknn}} if \code{precomputed=NULL}.}
}

\details{
This function uses the method proposed by Wang (2012) to quickly identify k-nearest neighbors in high-dimensional data.
Briefly, data points are rapidly clustered into \code{N} clusters using k-means clustering in \code{\link{buildKmknn}}, where \code{N} is the square root of the number of points.
This clustering is then used to speed up the nearest neighbor search across \code{X},
exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.

By default, nearest neighbors are identified for all data points within \code{X}.
If \code{subset} is specified, nearest neighbors are only detected for the points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findKmknn} with \code{subset=NULL}.

Turning off \code{get.index} or \code{get.distance} will not return the corresponding matrices in the output.
This may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search across multiple workers, which should increase speed proportionally (in theory) to the number of cores.

If the function is to be called multiple times with the same \code{X} (e.g., with different \code{subset}), 
it may be faster to call \code{\link{buildKmknn}} once externally, and pass the returned object to \code{\link{findKmknn}} via the \code{precomputed} argument.
This avoids unnecessary repeated k-means clustering and can provide a substantial speed-up.
Note that when \code{precomputed} is supplied, the value of \code{X} is completely ignored.

Currently, only Euclidean distances are supported, but support may be added for other distance types depending on demand.
It remains to be seen whether the speed-up achieved with k-means is still applicable to alternative distance metrics.

Note that the code here was originally derived from an implementation in the \pkg{cydar} package (Lun \emph{et al.}, 2017).
}

\section{Ties and random seeds}{
In general, this function is fully deterministic, despite the use of a stochastic \code{\link{kmeans}} step in \code{\link{buildKmknn}}.
The only exception occurs when there are tied distances to neighbors, at which point the order and/or identity of the k-nearest neighboring points is not well-defined.

A warning will be raised if ties are detected among the \code{k+1} nearest neighbors, as this indicates that the order/identity is arbitrary.
Specifically, ties are detected when a larger distance is less than \code{(1 + 1e-10)}-fold of the smaller distance.
This criterion tends to be somewhat conservative in the sense that it will warn users even if there is no problem (i.e., the distances are truly different).
However, more accurate detection is difficult to achieve due to the vagaries of numerical precision across different machines.

In the presence of ties, the output will depend on the ordering of points in the \code{\link{buildKmknn}} output.
Users should set the seed to guarantee consistent (albeit arbitrary) results across different runs of the function.
Note, however, that the exact selection of tied points depends on the numerical precision of the system.
Thus, even after setting a seed, there is no guarantee that the results will be reproducible across machines (especially Windows)!
}

\section{Returning raw indices}{
Advanced users can also set \code{raw.index=TRUE}, which yields results equivalent to running \code{findKmknn} on \code{t(PRE)} directly, 
where \code{PRE} is the output of \code{\link{KmknnIndex_clustered_data}(precomputed)}.
With this setting, the indices in the output \code{index} matrix refer to \emph{columns} of \code{PRE}.
Similarly, the \code{subset} argument is assumed to refer to columns of \code{PRE}.

This setting may be more convenient when the reordered data in \code{precomputed} is used elsewhere, e.g., for plotting.
With \code{raw.index=TRUE}, users avoid the need to switch between the original ordering and that from \code{\link{buildKmknn}}.
Of course, it is also the user's responsibility to be aware of the reordering in downstream applications.
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
    This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{X}.
    The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
    \item \code{distance}, if \code{get.distance=TRUE}.
    This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
}

If \code{subset} is not \code{NULL}, each row of the above matrices refers to a point in the subset, in the same order as supplied in \code{subset}.

If \code{raw.index=TRUE}, the values in \code{index} refer to \emph{columns} of \code{\link{KmknnIndex_clustered_data}(precomputed)}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} to build the index structure ahead of time.
}

\references{
Wang X (2012). 
A fast exact k-nearest neighbors algorithm for high dimensional search using k-means clustering and triangle inequality. 
\emph{Proc Int Jt Conf Neural Netw}, 43, 6:2351-2358.

Lun ATL, Richard AC, Marioni JC (2017). 
Testing for differential abundance in mass cytometry data. 
\emph{Nat. Methods}, 14, 7:707-709.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- findKmknn(Y, k=25)
head(out$index)
head(out$distance)
}
