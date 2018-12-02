\name{findVptree}
\alias{findVptree}

\title{Find nearest neighbors}
\description{Use the vantage point (VP) tree to identify nearest neighbors from a dataset.}

\usage{
findVptree(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, raw.index=FALSE, ...)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{BPPARAM}{A \linkS4class{BiocParallelParam} object indicating how the search should be parallelized.}
\item{precomputed}{A \linkS4class{VptreeIndex} object returned from running \code{\link{buildVptree}} on \code{X}.}
\item{subset}{A vector indicating the rows of \code{X} for which the nearest neighbors should be identified.}
\item{raw.index}{A logical scalar indicating whether column indices to the reordered data in \code{precomputed} should be directly returned.}
}

\details{
This function uses a VP tree to quickly identify k-nearest neighbors in high-dimensional data.
In the VP tree, each node contains a subset of points and has a defined threshold distance (usually the median distance to all points in the subset).
The left child contains the further subset of points within the radius, while the right child contains the remaining points in the subset that are outside.
The nearest neighbor search across \code{X} exploits the triangle inequality between node centers and thresholds to narrow the search space.

By default, nearest neighbors are identified for all data points within \code{X}.
If \code{subset} is specified, nearest neighbors are only detected for the points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findVptree} with \code{subset=NULL}.

Turning off \code{get.index} or \code{get.distance} will not return the corresponding matrices in the output.
This may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search across multiple workers, which should increase speed proportionally (in theory) to the number of cores.

If the function is to be called multiple times with the same \code{X} (e.g., with different \code{subset}), 
it may be faster to call \code{\link{buildVptree}} once externally, and pass the returned object to \code{\link{findVptree}} via the \code{precomputed} argument.
This avoids unnecessary repeated tree building and can provide a substantial speed-up.
Note that when \code{precomputed} is supplied, the value of \code{X} is completely ignored.
}

\section{Returning raw indices}{
Advanced users can also set \code{raw.index=TRUE}, which yields results equivalent to running \code{findVptree} on \code{t(PRE)} directly, 
where \code{PRE} is the output of \code{\link{VptreeIndex_data}(precomputed)}.
With this setting, the indices in the output \code{index} matrix refer to \emph{columns} of \code{PRE}.
Similarly, the \code{subset} argument is assumed to refer to columns of \code{PRE}.

This setting may be more convenient when the reordered data in \code{precomputed} is used elsewhere, e.g., for plotting.
With \code{raw.index=TRUE}, users avoid the need to switch between the original ordering and that from \code{\link{buildVptree}}.
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

If \code{raw.index=TRUE}, the values in \code{index} refer to \emph{columns} of \code{\link{VptreeIndex_clustered_data}(precomputed)}.
}

\author{
Aaron Lun, based on C++ code by Steve Hanov.
}

\seealso{
\code{\link{buildVptree}} to build the index structure ahead of time.
}

\references{
Hanov S (2011).
VP trees: A data structure for finding stuff fast.
\url{http://stevehanov.ca/blog/index.php?id=130}

Yianilos PN (1993).
Data structures and algorithms for nearest neighbor search in general metric spaces.
\emph{Proceedings of the Fourth Annual ACM-SIAM Symposium on Discrete Algorithms}, 311-321 
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- findVptree(Y, k=25)
head(out$index)
head(out$distance)
}
