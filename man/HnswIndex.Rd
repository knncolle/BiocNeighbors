\name{HnswIndex}
\alias{HnswIndex-class}

\alias{HnswIndex}
\alias{show,HnswIndex-method}
\alias{HnswIndex_path}

\title{The HnswIndex class}
\description{A class to hold indexing structures for the HNSW algorithm for approximate nearest neighbor identification.}

\usage{
HnswIndex(data, path, NAMES=NULL)

HnswIndex_path(x)

\S4method{show}{HnswIndex}(object)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{path}{A string specifying the path to the index file.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{x, object}{A HnswIndex object.}
}

\details{
The HnswIndex class holds the indexing structure required to run the HNSW algorithm.
Users should never need to call the constructor explicitly, but should generate instances of HnswIndex classes with \code{\link{buildHnsw}}.
}

\value{
The \code{HnswIndex} constructor will return an instance of the HnswIndex class.

\code{HnswIndex_path} will return the path to the index file.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildHnsw}} 
}

\examples{
example(buildHnsw)
str(HnswIndex_path(out))
}
