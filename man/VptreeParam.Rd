\name{VptreeParam}
\alias{VptreeParam}
\alias{VptreeParam-class}

\title{The VptreeParam class}
\description{A class to hold parameters for the VP tree algorithm for exact nearest neighbor identification.}

\usage{
VptreeParam(distance="Euclidean")
}

\arguments{
\item{distance}{A string specifying the distance metric to use.}
}

\value{
The \code{VptreeParam} constructor will return an instance of the VptreeParam class.
}

\author{
Aaron Lun
}

\seealso{
\code{\link{buildVptree}} 
}

\examples{
(out <- VptreeParam())
}
