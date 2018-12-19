\name{HnswParam}
\alias{HnswParam}
\alias{HnswParam-class}
\alias{show,HnswParam-method}
\alias{HnswParam_nlinks}
\alias{HnswParam_ef_construction}
\alias{HnswParam_directory}

\title{The HnswParam class}
\description{A class to hold parameters for the Hnsw algorithm for approximate nearest neighbor identification.}

\usage{
HnswParam(nlinks=16, ef.construction=200, directory=tempdir(), distance="Euclidean")

HnswParam_nlinks(x)

HnswParam_ef_construction(x)

HnswParam_directory(x)

\S4method{show}{HnswParam}(object)
}

\arguments{
    \item{nlinks}{Integer scalar, number of bi-directional links per element for index generation.}
    \item{ef.construction}{Integer scalar, size of the dynamic list for index generation.}
    \item{directory}{String specifying the directory in which to save the index.}
    \item{distance}{A string specifying the distance metric to use.}
    \item{x, object}{A HnswParam object.}
}

\details{
The HnswParam class holds any parameters associated with running the HNSW algorithm.
Currently, this relates to building of the index - see \code{\link{buildHnsw}} for details.
}

\value{
The \code{HnswParam} constructor will return an instance of the HnswParam class.

\code{HnswParam_nlinks} and \code{HnswParam_ef_construction} will return the number of links and the size of the dynamic list, respectively, as integer scalars.

\code{HnswParam_directory} will return the directory as a string.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildHnsw}} 
}

\examples{
(out <- HnswParam())

HnswParam_nlinks(out)
HnswParam_ef_construction(out)
HnswParam_directory(out)
}
