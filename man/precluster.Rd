\name{precluster}
\alias{precluster}

\title{Pre-cluster points with k-means}
\description{Perform k-means clustering in preparation for a nearest-neighbors search.}

\usage{
precluster(X, ...)
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{...}{Further arguments to pass to \code{\link{kmeans}}.}
}

\details{
This function is automatically called by \code{\link{findKNN}} and related functions. 
However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.
}

\value{
A list containing:
\itemize{
\item \code{data}, a numeric matrix with points in the \emph{columns} and dimensions in the rows, i.e., transposed relative to the input.
Points have also been reordered to improve data locality during the nearest-neighbor search.
Specifically, points in the same cluster are contiguous and ordered by increasing distance from the cluster center.
\item \code{clusters}, itself a list containing:
\itemize{
  \item \code{centers}, a numeric matrix of cluster center coordinates where each column corresponds to a cluster.
  \item \code{info}, another list of length equal to the number of clusters.
  Each entry corresponds to a column of \code{centers} (let's say cluster \eqn{j}) and is a list of length 2.
  The first element is the zero-index of the first cell in the \emph{output} \code{X} that is assigned to \eqn{j}.
  The second element is the distance of each point in the cluster from the cluster center.
}
\item \code{order}, an integer vector specifying the new ordering of points in \code{X}.
}
}

\seealso{
\code{\link{kmeans}},
\code{\link{findKNN}},
\code{\link{queryKNN}},
\code{\link{findNeighbors}}
}

\author{
Aaron Lun
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- precluster(Y)
str(out)
}

