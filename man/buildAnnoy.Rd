\name{buildAnnoy}
\alias{buildAnnoy}

\title{Build an Annoy index}
\description{Build an Annoy index and save it to file in preparation for a nearest-neighbors search.}

\usage{
buildAnnoy(X, transposed=FALSE, ntrees=50, directory=tempdir(),
    fname=tempfile(tmpdir=directory, fileext=".idx")) 
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{transposed}{Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.}
\item{ntrees}{Integer scalar specifying the number of trees, where more trees provide greater accuracy at the cost of more computational work.}
\item{directory}{String containing the path to the directory in which to save the index file.}
\item{fname}{String containing the path to the index file.}
}

\details{
This function is automatically called by \code{\link{findAnnoy}} and related functions. 
However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.

It is advisable to change \code{directory} to a location that is amenable to parallel read operations on HPC file systems.
Of course, if index files are manually constructed, the user is also responsible for their clean-up after all calculations are completed.
}

\value{
A \linkS4class{AnnoyIndex} object containing:
\itemize{
\item \code{path}, a string containing the path to the index file.
\item \code{dim}, an integer vector of length 2 containing \code{dim(X)} for later reference.
\item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
}
}

\seealso{
See \code{\link{AnnoyIndex}} for details on the output class.

See \code{\link{findAnnoy}} and \code{\link{queryAnnoy}} for dependent functions.
}

\author{
Aaron Lun
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- buildAnnoy(Y)
out
}

