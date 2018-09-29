\name{KmknnParam}
\alias{KmknnParam}
\alias{KmknnParam-class}
\alias{show,KmknnParam-method}
\alias{KmknnParam_kmeans_args}

\title{The KmknnParam class}
\description{A class to hold parameters for the KMKNN algorithm for exact nearest neighbor identification.}

\usage{
KmknnParam(...)

KmknnParam_kmeans_args(x)

\S4method{show}{KmknnParam}(object)
}

\arguments{
    \item{...}{Arguments to be passed to \code{\link{kmeans}}.}
    \item{x, object}{A KmknnParam object.}
}

\details{
The KmknnParam class holds any parameters associated with running the KMKNN algorithm.
Currently, this relates to tuning of the k-means step - see \code{\link{buildKmknn}} for details.
}

\value{
The \code{KmknnParam} constructor will return an instance of the KmknnParam class.

The \code{KmknnParam_kmeans_args} function will return a list of named arguments, used in \code{...} to construct \code{object}.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildKmknn}} 
}

\examples{
(out <- KmknnParam(iter.max=100))

KmknnParam_kmeans_args(out)
}
