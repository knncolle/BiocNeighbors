\name{KmknnIndex}
\alias{KmknnIndex}
\alias{KmknnIndex-class}

\alias{show,KmknnIndex-method}
\alias{dim,KmknnIndex-method}
\alias{dimnames,KmknnIndex-method}

\alias{KmknnIndex_clustered_data}
\alias{KmknnIndex_cluster_centers}
\alias{KmknnIndex_cluster_info}
\alias{KmknnIndex_clustered_order}

\title{The KmknnIndex class}
\description{A class to hold indexing structures for the KMKNN algorithm for exact nearest neighbor identification.}

\usage{
KmknnIndex(data, centers, info, order, NAMES=NULL)

KmknnIndex_clustered_data(x)

KmknnIndex_cluster_centers(x)

KmknnIndex_cluster_info(x)

KmknnIndex_clustered_order(x)

\S4method{show}{KmknnIndex}(object)

\S4method{dim}{KmknnIndex}(x)

\S4method{dimnames}{KmknnIndex}(x)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{centers}{A numeric matrix with clusters in columns and dimensions in rows.}
    \item{info}{A list of statistics for each cluster.}
    \item{order}{An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{x, object}{A KmknnIndex object.}
}

\details{
The KmknnIndex class holds the indexing structure required to run the KMKNN algorithm.
Users should never need to call the constructor explicitly, but should generate instances of KmknnIndex classes with \code{\link{buildKmknn}}.
}

\value{
The \code{KmknnIndex} constructor will return an instance of the KmknnIndex class.

\code{KmknnIndex_clustered_data} and related getters will return the corresponding slots of \code{object}.

\code{dim} will return the dimensions of \code{t(data)}.

\code{dimnames} will return a list of length 2 where the first element is the supplied \code{NAMES}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} 
}

\examples{
example(buildKmknn)

dim(out)

str(KmknnIndex_clustered_data(out))
str(KmknnIndex_cluster_centers(out))
str(KmknnIndex_clustered_order(out))
}
