\name{find.knn}
\alias{find.knn}

\title{Find nearest neighbors}
\description{Use the KMKNN (K-means for k-nearest neighbors) algorithm to identify nearest neighbors from a dataset.}

\usage{
find.knn(X, k, get.index=TRUE, get.distance=TRUE, query=NULL, 
    BPPARAM=SerialParam(), precomputed=NULL, query.transposed=FALSE)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{query}{A numeric matrix of query points, containing different data points in the rows but the same number and ordering of dimensions in the columns.}
\item{BPPARAM}{A BiocParallelParam object indicating how the search should be parallelized.}
\item{precomputed}{The precomputed output of \code{\link{precluster}} on \code{X}.}
\item{query.transposed}{A logical scalar indicating whether the \code{query} is transposed, 
in which case \code{query} is assumed to contain dimensions in the rows and data points in the columns.}
}

\details{
This function uses the method proposed by Wang (2012) to quickly identify k-nearest neighbors in high-dimensional data.
Briefly, data points are rapidly clustered into \code{N} clusters using k-means clustering in \code{\link{precluster}}, where \code{N} is the square root of the number of points.
This clustering is then used to speed up the nearest neighbor search across \code{X},
exploiting the triangle inequality between cluster centers, the query point and each point in the cluster to narrow the search space.

By default, nearest neighbors are identified for all data points within \code{X}.
Users can also identify points in \code{X} that are nearest neighbors of each point in \code{query}.
This requires both \code{X} and \code{query} to have the same number of dimensions.
If \code{query.transposed=TRUE}, this function assumes that \code{query} is already transposed, which saves a bit of time.

When \code{query=NULL}, the function automatically caps \code{k} at the number of points in \code{X} minus 1, and emits a warning when this occurs.
If \code{query} is not \code{NULL}, the upper bound for \code{k} is set at the number of points in \code{X}.

Turning off \code{get.index} or \code{get.distance} may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search by query points, which usually provides a linear increase in speed.

If multiple queries are to be performed to the same \code{X}, it may be beneficial to use \code{\link{precluster}} directly to precompute the clustering.
This can be re-used by supplying the output of \code{\link{precluster}} to \code{precomputed}, which often provides a substantial speed-up.

Currently, only Euclidean distances are supported.
Support may be added for other distance types, depending on demand.

Note that the code here was originally derived from an implementation in the \pkg{cydar} package (Lun \emph{et al.}, 2017).
}

\value{
A list is returned containing:
\itemize{
    \item \code{index}, if \code{get.index=TRUE}.
    This is an integer matrix where each row corresponds to a point (denoted here as \eqn{i}) in \code{X} (or in \code{query}, if not \code{NULL}).
    The row for \eqn{i} contains the row indices of \code{X} that are the nearest neighbors to point \eqn{i}, sorted by increasing distance from \eqn{i}.
    \item \code{distance}, if \code{get.distance=TRUE}.
    This is a numeric matrix where each row corresponds to a point (as above) and contains the sorted distances of the neighbors from \eqn{i}.
}
}

\author{
Aaron Lun
}

\seealso{
\code{\link{precluster}}
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
out <- find.knn(Y, k=25)
head(out$index)
head(out$distance)
}
