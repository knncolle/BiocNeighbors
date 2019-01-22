\name{AnnoyIndex}
\alias{AnnoyIndex-class}

\alias{AnnoyIndex}
\alias{show,AnnoyIndex-method}
\alias{AnnoyIndex_path}
\alias{AnnoyIndex_search_mult}

\title{The AnnoyIndex class}
\description{A class to hold indexing structures for the Annoy algorithm for approximate nearest neighbor identification.}

\usage{
AnnoyIndex(data, path, search.mult=50, NAMES=NULL, distance="Euclidean")

AnnoyIndex_path(x)

\S4method{show}{AnnoyIndex}(object)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{path}{A string specifying the path to the index file.}
    \item{search.mult}{Numeric scalar, multiplier for the number of points to search.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{distance}{A string specifying the distance metric to use.}
    \item{x, object}{A AnnoyIndex object.}
}

\details{
The AnnoyIndex class holds the indexing structure required to run the Annoy algorithm.
Users should never need to call the constructor explicitly, but should generate instances of AnnoyIndex classes with \code{\link{buildAnnoy}}.
}

\value{
The \code{AnnoyIndex} constructor will return an instance of the AnnoyIndex class.

\code{AnnoyIndex_path} will return the path to the index file.

\code{AnnoyIndex_search_mult} will return the multiplier for the number of points to search.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildAnnoy}} 
}

\examples{
example(buildAnnoy)
str(AnnoyIndex_path(out))
}
