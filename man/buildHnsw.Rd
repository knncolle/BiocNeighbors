\name{buildHnsw}
\alias{buildHnsw}

\title{Build a HNSW index}
\description{Build a HNSW index and save it to file in preparation for a nearest-neighbors search.}

\usage{
buildHnsw(X, transposed=FALSE, nlinks=16, ef.construction=200,
    directory=tempdir(), fname=tempfile(tmpdir=directory, fileext=".idx"))
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{transposed}{Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.}
\item{nlinks}{Integer scalar specifying the number of bi-directional links for each element.}
\item{ef.construction}{Integer scalar specifying the size of the dynamic list during construction.}
\item{directory}{String containing the path to the directory in which to save the index file.}
\item{fname}{String containing the path to the index file.}
}

\details{
This function is automatically called by \code{\link{findHnsw}} and related functions. 
However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.

It is advisable to change \code{directory} to a location that is amenable to parallel read operations on HPC file systems.
Of course, if index files are manually constructed, the user is also responsible for their clean-up after all calculations are completed.

Larger values of \code{nlinks} improve accuracy at the expense of speed and memory usage.
Larger values of \code{ef.construction} improve index quality at the expense of indexing time.

Technically, the index construction algorithm is stochastic but, for various logistical reasons, the seed is hard-coded into the C++ code.
This means that the results of the HNSW neighbor searches will be fully deterministic for the same inputs, even though the theory provides no such guarantees.
}

\value{
A \linkS4class{HnswIndex} object containing:
\itemize{
\item \code{path}, a string containing the path to the index file.
\item \code{data}, a numeric matrix equivalent to \code{t(X)}.
\item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
}
}

\seealso{
See \code{\link{HnswIndex}} for details on the output class.

See \code{\link{findHnsw}} and \code{\link{queryHnsw}} for dependent functions.
}

\author{
Aaron Lun
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- buildHnsw(Y)
out
}

