\name{HnswIndex}
\alias{HnswIndex-class}

\alias{HnswIndex}
\alias{show,HnswIndex-method}
\alias{HnswIndex_path}
\alias{HnswIndex_ef_search}

\title{The HnswIndex class}
\description{A class to hold indexing structures for the HNSW algorithm for approximate nearest neighbor identification.}

\usage{
HnswIndex(data, path, ef.search=200, NAMES=NULL, distance="Euclidean")

HnswIndex_path(x)

HnswIndex_ef_search(x)

\S4method{show}{HnswIndex}(object)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{path}{A string specifying the path to the index file.}
    \item{ef.search}{Integer scalar specifying the size of the dynamic list at run time.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{distance}{A string specifying the distance metric to use.}
    \item{x, object}{A HnswIndex object.}
}

\details{
The HnswIndex class holds the indexing structure required to run the HNSW algorithm.
Users should never need to call the constructor explicitly, but should generate instances of HnswIndex classes with \code{\link{buildHnsw}}.
}

\value{
The \code{HnswIndex} constructor will return an instance of the HnswIndex class.

\code{HnswIndex_path} will return the path to the index file.

\code{HnswIndex_ef_search} will return the size of the dynamic list.
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
