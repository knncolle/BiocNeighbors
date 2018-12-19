\name{AnnoyParam}
\alias{AnnoyParam}
\alias{AnnoyParam-class}
\alias{show,AnnoyParam-method}
\alias{AnnoyParam_ntrees}
\alias{AnnoyParam_directory}

\title{The AnnoyParam class}
\description{A class to hold parameters for the Annoy algorithm for approximate nearest neighbor identification.}

\usage{
AnnoyParam(ntrees=50, directory=tempdir(), distance="Euclidean")

AnnoyParam_ntrees(x)

AnnoyParam_directory(x)

\S4method{show}{AnnoyParam}(object)
}

\arguments{
    \item{ntrees}{Integer scalar, number of trees to use for index generation.}
    \item{directory}{String specifying the directory in which to save the index.}
    \item{distance}{A string specifying the distance metric to use.}
    \item{x, object}{A AnnoyParam object.}
}

\details{
The AnnoyParam class holds any parameters associated with running the Annoy algorithm.
Currently, this relates to building of the index - see \code{\link{buildAnnoy}} for details.
}

\value{
The \code{AnnoyParam} constructor will return an instance of the AnnoyParam class.

\code{AnnoyParam_ntrees} will return the number of trees as an integer scalar.

\code{AnnoyParam_directory} will return the directory as a string.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildAnnoy}} 
}

\examples{
(out <- AnnoyParam())

AnnoyParam_ntrees(out)
AnnoyParam_directory(out)
}
