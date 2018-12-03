\name{VptreeIndex}
\alias{VptreeIndex-class}

\alias{VptreeIndex}
\alias{VptreeIndex_nodes}

\title{The VptreeIndex class}
\description{A class to hold the vantage point tree for exact nearest neighbor identification.}

\usage{
VptreeIndex(data, nodes, order, NAMES=NULL)

VptreeIndex_nodes(x)
}

\arguments{
    \item{data}{A numeric matrix with data points in columns and dimensions in rows.}
    \item{nodes}{A list of vectors specifying the structure of the VP tree.}
    \item{order}{An integer vector of length equal to \code{ncol(data)}, specifying the order of observations.}
    \item{NAMES}{A character vector of sample names or \code{NULL}.}
    \item{x}{A VptreeIndex object.}
}

\details{
The VptreeIndex class holds the indexing structure required to run the VP tree algorithm.
Users should never need to call the constructor explicitly, but should generate instances of VptreeIndex classes with \code{\link{buildVptree}}.
}

\value{
The \code{VptreeIndex} constructor will return an instance of the VptreeIndex class.

\code{VptreeIndex_nodes} will return the corresponding slots of \code{x}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildVptree}} 
}

\examples{
example(buildVptree)
str(VptreeIndex_nodes(out))
}
