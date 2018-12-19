\name{buildVptree}
\alias{buildVptree}

\title{Build a VP tree}
\description{Build a vantage point tree in preparation for a nearest-neighbors search.}

\usage{
buildVptree(X, transposed=FALSE, distance=c("Euclidean", "Manhattan"))
}

\arguments{
\item{X}{A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).}
\item{transposed}{Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.}
\item{distance}{String specifying the type of distance to use.}
}

\details{
This function is automatically called by \code{\link{findVptree}} and related functions. 
However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.
}

\value{
A \linkS4class{VptreeIndex} object containing:
\itemize{
\item \code{data}, a numeric matrix with points in the \emph{columns} and dimensions in the rows, i.e., transposed relative to the input.
Points have also been reordered to improve data locality during the nearest-neighbor search.
\item \code{nodes}, a list containing four vectors of equal length describing the structure of the VP tree.
Each parallel element specifies a node.
\itemize{
    \item The first integer vector specifies the column index of \code{data} of the current node.
    \item The second integer vector specifies the column index of the left child of the current node, 
    \item The third integer vector specifies the column index of the right child of the current node.
    \item The fourth numeric vector specifies the radius of the current node.
}
All indices here are zero-based, with child values set to -1 for leaf nodes.
\item \code{order}, an integer vector specifying how rows in \code{X} have been reordered in columns of \code{data}.
\item \code{NAMES}, a character vector or \code{NULL} equal to \code{rownames(X)}.
}
}

\seealso{
See \code{\link{VptreeIndex}} for details on the output class.

See \code{\link{findVptree}} and \code{\link{queryVptree}} for dependent functions.
}

\author{
Aaron Lun
}

\examples{
Y <- matrix(rnorm(100000), ncol=20)
out <- buildVptree(Y)
out
}

