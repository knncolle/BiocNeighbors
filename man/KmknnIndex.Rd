\name{KmknnIndex}
\alias{KmknnIndex-class}

\alias{KmknnIndex}
\alias{KmknnIndex_cluster_centers}
\alias{KmknnIndex_cluster_info}
\alias{show,KmknnIndex-method}

\title{The KmknnIndex class}
\description{A class to hold indexing structures for the KMKNN algorithm for exact nearest neighbor identification.}

\usage{
KmknnIndex(data, centers, info, order, NAMES=NULL, distance="Euclidean")

KmknnIndex_cluster_centers(x)

KmknnIndex_cluster_info(x)

\S4method{show}{KmknnIndex}(object)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{centers}{A numeric matrix with clusters in columns and dimensions in rows.}
    \item{info}{A list of statistics for each cluster.}
    \item{order}{An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{distance}{A string specifying the distance metric to use.}
    \item{x, object}{A KmknnIndex object.}
}

\details{
The KmknnIndex class holds the indexing structure required to run the KMKNN algorithm.
Users should never need to call the constructor explicitly, but should generate instances of KmknnIndex classes with \code{\link{buildKmknn}}.
}

\value{
The \code{KmknnIndex} constructor will return an instance of the KmknnIndex class.

\code{KmknnIndex_cluster_centers} and related getters will return the corresponding slots of \code{object}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} 
}

\examples{
example(buildKmknn)
str(KmknnIndex_cluster_centers(out))
str(KmknnIndex_cluster_info(out))
}
