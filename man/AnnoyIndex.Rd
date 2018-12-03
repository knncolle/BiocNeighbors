\name{AnnoyIndex}
\alias{AnnoyIndex-class}

\alias{AnnoyIndex}
\alias{show,AnnoyIndex-method}
\alias{AnnoyIndex_path}

\title{The AnnoyIndex class}
\description{A class to hold indexing structures for the Annoy algorithm for approximate nearest neighbor identification.}

\usage{
AnnoyIndex(data, path, NAMES=NULL)

AnnoyIndex_path(x)

\S4method{show}{AnnoyIndex}(object)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{path}{A string specifying the path to the index file.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{x, object}{A AnnoyIndex object.}
}

\details{
The AnnoyIndex class holds the indexing structure required to run the KMKNN algorithm.
Users should never need to call the constructor explicitly, but should generate instances of AnnoyIndex classes with \code{\link{buildAnnoy}}.
}

\value{
The \code{AnnoyIndex} constructor will return an instance of the AnnoyIndex class.

\code{AnnoyIndex_path} will return the path to the index file.
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
