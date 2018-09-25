\name{findAnnoy}
\alias{findAnnoy}

\title{Find approximate nearest neighbors}
\description{Use the Annoy algorithm to identify approximate nearest neighbors from a dataset.}

\usage{
findAnnoy(X, k, get.index=TRUE, get.distance=TRUE, BPPARAM=SerialParam(), 
    precomputed=NULL, subset=NULL, ntrees=50)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{k}{A positive integer scalar specifying the number of nearest neighbors to retrieve.}
\item{get.index}{A logical scalar indicating whether the indices of the nearest neighbors should be recorded.}
\item{get.distance}{A logical scalar indicating whether distances to the nearest neighbors should be recorded.}
\item{BPPARAM}{A BiocParallelParam object indicating how the search should be parallelized.}
\item{precomputed}{A string containing the file path of the index, returned by \code{\link{buildAnnoy}} on \code{X}.}
\item{subset}{A vector indicating the rows of \code{X} for which the nearest neighbors should be identified.}
\item{ntrees}{Integer scalar specifying the number of trees to use in \code{\link{buildAnnoy}} if \code{precomputed=NULL}.}
}

\details{
This function uses the Annoy method (see \url{https://github.com/spotify/annoy}) by Erik Bernhardsson to identify approximate k-nearest neighbors in high-dimensional data.
Briefly, a tree is constructed by using a random hyperplane to split the points at each internal node.
For a given input data point, all points in the same leaf node are defined as a set of potential nearest neighbors.
This is repeated across \code{ntrees} trees, and the union of all such sets is searched to identify the actual nearest neighbors.

By default, nearest neighbors are identified for all data points within \code{X}.
If \code{subset} is specified, nearest neighbors are only detected for the points in the subset.
This yields the same result as (but is more efficient than) subsetting the output matrices after running \code{findKNN} with \code{subset=NULL}.

Turning off \code{get.index} or \code{get.distance} will not return the corresponding matrices in the output.
This may provide a slight speed boost when these returned values are not of interest.
Using \code{BPPARAM} will also split the search across multiple workers, which should increase speed proportionally (in theory) to the number of cores.

If the function is to be called multiple times with the same \code{X} (e.g., with different \code{subset}), 
it may be faster to call \code{\link{buildAnnoy}} once externally, and pass the returned object to \code{findAnnoy} via the \code{precomputed} argument.
This avoids unnecessary re-indexing and can provide a substantial speed-up.
Note that when \code{precomputed} is supplied, the value of \code{X} is completely ignored.

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
}

\author{
Aaron Lun
}

\seealso{
See \code{\link{buildAnnoy}} to construct the index files ahead of time.
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- findAnnoy(Y, k=25)
head(out$index)
head(out$distance)
}
