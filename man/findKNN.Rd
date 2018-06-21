\name{findKNN}
\alias{findKNN}

\title{Find nearest neighbors}
\description{Use the KMKNN (K-means for k-nearest neighbors) algorithm to identify nearest neighbors from a dataset.}

\usage{
findKNN(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, raw.index=FALSE)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{BPPARAM}{A BiocParallelParam object indicating how the search should be parallelized.}
\item{precomputed}{The precomputed output of \code{\link{precluster}} on \code{X}.}
\item{subset}{A vector indicating the rows of \code{X} for which the nearest neighbors should be identified.}
\item{raw.index}{A logial scalar indicating whether raw column indices to \code{precomputed$data} should be returned.}
}

\details{
This function uses the method proposed by Wang (2012) to quickly identify k-nearest neighbors in high-dimensional data.
Briefly, data points are rapidly clustered into \code{N} clusters using k-means clustering in \code{\link{precluster}}, where \code{N} is the square root of the number of points.
This clustering is then used to speed up the nearest neighbor search across \code{X},
exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.

By default, nearest neighbors are identified for all data points within \code{X}.
If \code{subset} is specified, nearest neighbors are only detected for the points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findKNN} with \code{subset=NULL}.

Turning off \code{get.index} or \code{get.distance} may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search by query points across multiple processes.

If the function is to be called multiple times with the same \code{X} (e.g., with different \code{subset}), 
it may be beneficial to call \code{\link{precluster}} outside of this function.
The output can then be supplied to the \code{precomputed} argument across multiple calls to \code{findKNN}.

Note that when \code{precomputed} is supplied, the value of \code{X} is ignored.
Advanced users can also set \code{raw.index=TRUE}, which yields results equivalent to running \code{findKNN} on \code{t(precomputed$data)} directly.
The output indices refer to \emph{columns} of \code{precomputed$data}, which may be more convenient when dealing with a common \code{precomputed} object. 

Currently, only Euclidean distances are supported, but support may be added for other distance types depending on demand.
It remains to be seen whether the speed-up achieved with k-means is still applicable to alternative distance metrics.

Note that the code here was originally derived from an implementation in the \pkg{cydar} package (Lun \emph{et al.}, 2017).
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

If \code{raw.index=TRUE}, the values in \code{index} refer to \emph{columns} of \code{precomputed$data}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{precluster}},
\code{\link{queryKNN}}
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
out <- findKNN(Y, k=25)
head(out$index)
head(out$distance)
}
