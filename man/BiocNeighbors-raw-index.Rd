\name{Raw indices}
\alias{BiocNeighbors-raw-index}

\title{Reporting raw indices}
\description{An overview of what raw indices mean for neighbor-search implementations that contain a rearranged matrix in the \linkS4class{BiocNeighborIndex} object.}

\section{What are raw indices?}{
Consider the following call:
\preformatted{    index <- buildKmknn(vals)    
    out <- findKmknn(precomputed=index, raw.index=TRUE)
}
This yields the same output as:
\preformatted{    PRE <- bndata(index)
    out2 <- findKmknn(X=t(PRE))
}

When \code{raw.index=TRUE} in the first call, the indices in \code{out$index} matrix can be imagined to refer to \emph{columns} of \code{PRE} in the second call.
Similarly, specifying the \code{subset} argument in the first call is assumed to (conceptually) refer to columns of \code{PRE}.

The same reasoning applies to all functions where \code{precomputed} can be specified in place of \code{X}.
This includes query-based searches (e.g., \code{\link{queryKmknn}}) and range searches (\code{\link{rangeFindKmknn}}).
}

\section{Motivation}{ 
This setting may be more convenient when the reordered data in \code{precomputed} is used elsewhere, e.g., for plotting.
With \code{raw.index=TRUE}, users avoid the need to switch between the original ordering and that in the build index. 
Of course, it is also the user's responsibility to be aware of the reordering in downstream applications.

Neighbor search implementations can only return raw indices if their index construction involves transposing \code{X} and reordering its columns.
This tends to be the case for most implementations as transposition allows efficient column-major distance calculations and reordering improves data locality.
Both the KMKNN and VP tree implementations fulfill these requirements, and thus have the \code{raw.index} option.

Note that setting \code{raw.index=TRUE} makes little sense when \code{precomputed} is not specified.
When \code{precomputed=NULL}, a temporary index will be constructed that is not visible in the calling scope.
As index construction may be stochastic, the raw indices will not refer to anything meaningful to the end-user.
}

\seealso{
\code{\link{findKmknn}} and \code{\link{findVptree}} for examples where raw indices are used.
}

\author{
Aaron Lun
}

\examples{
vals <- matrix(rnorm(100000), ncol=20)
index <- buildKmknn(vals)    
out <- findKmknn(precomputed=index, raw.index=TRUE, k=5)
alt <- findKmknn(t(bndata(index)), k=5)    
head(out$index)
head(alt$index)
}
