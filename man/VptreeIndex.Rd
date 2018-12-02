\name{VptreeIndex}
\alias{VptreeIndex}
\alias{VptreeIndex-class}

\alias{dim,VptreeIndex-method}
\alias{VptreeIndex_data}
\alias{VptreeIndex_nodes}
\alias{VptreeIndex_order}

\title{The VptreeIndex class}
\description{A class to hold the vantage point tree for exact nearest neighbor identification.}

\usage{
VptreeIndex(data, order, nodes, NAMES=NULL)

VptreeIndex_data(x)

VptreeIndex_nodes(x)

VptreeIndex_order(x)

\S4method{dim}{VptreeIndex}(x)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{order}{An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.}
    \item{nodes}{A list of vectors specifying the structure of the VP tree.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
}

\details{
The VptreeIndex class holds the indexing structure required to run the KMKNN algorithm.
Users should never need to call the constructor explicitly, but should generate instances of VptreeIndex classes with \code{\link{buildVptree}}.
}

\value{
The \code{VptreeIndex} constructor will return an instance of the VptreeIndex class.

\code{VptreeIndex_data} and related getters will return the corresponding slots of \code{x}.

\code{dim} will return the dimensions of \code{t(data)}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildVptree}} 
}

\examples{
example(buildVptree)

dim(out)

str(VptreeIndex_data(out))
str(VptreeIndex_nodes(out))
str(VptreeIndex_order(out))
}
