\name{AnnoyIndex}
\alias{AnnoyIndex}
\alias{AnnoyIndex-class}

\alias{show,AnnoyIndex-method}
\alias{dim,AnnoyIndex-method}
\alias{dimnames,AnnoyIndex-method}

\alias{AnnoyIndex_path}

\title{The AnnoyIndex class}
\description{A class to hold indexing structures for the Annoy algorithm for approximate nearest neighbor identification.}

\usage{
AnnoyIndex(path, dim, NAMES=NULL)

AnnoyIndex_path(x)

\S4method{show}{AnnoyIndex}(object)

\S4method{dim}{AnnoyIndex}(x)

\S4method{dimnames}{AnnoyIndex}(x)
}

\arguments{
    \item{path}{A string specifying the path to the index file.}
    \item{dim}{An integer vector of length 2, specifying the dimensions of the data used to construct the index.}
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

\code{dim} will return the dimensions of the data set used to create the index.

\code{dimnames} will return a list of length 2 where the first element is the supplied \code{NAMES}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildAnnoy}} 
}

\examples{
example(buildAnnoy)

dim(out)
str(AnnoyIndex_path(out))
}
