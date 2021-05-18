#' Build a HNSW index
#' 
#' Build a HNSW index and save it to file in preparation for a nearest-neighbors search.
#' 
#' @param X A numeric matrix where rows correspond to data points and columns correspond to variables (i.e., dimensions).
#' @param transposed Logical scalar indicating whether \code{X} is transposed, i.e., rows are variables and columns are data points.
#' @param nlinks Integer scalar specifying the number of bi-directional links for each element.
#' @param ef.construction Integer scalar specifying the size of the dynamic list during index construction.
#' @param directory String containing the path to the directory in which to save the index file.
#' @param ef.search Integer scalar specifying the size of the dynamic list to use during neighbor searching.
#' @param fname String containing the path to the index file.
#' @param distance String specifying the type of distance to use.
#' 
#' @details
#' This function is automatically called by \code{\link{findHnsw}} and related functions. 
#' However, it can be called directly by the user to save time if multiple queries are to be performed to the same \code{X}.
#' 
#' It is advisable to change \code{directory} to a location that is amenable to parallel read operations on HPC file systems.
#' Of course, if index files are manually constructed, the user is also responsible for their clean-up after all calculations are completed.
#' 
#' Larger values of \code{nlinks} improve accuracy at the expense of speed and memory usage.
#' Larger values of \code{ef.construction} improve index quality at the expense of indexing time.
#' 
#' The value of \code{ef.search} controls the accuracy of the neighbor search at run time.
#' Larger values improve accuracy at the expense of a slower search.
#' In \code{\link{findHnsw}} and \code{\link{queryHnsw}}, this is always lower-bounded at \code{k}, the number of nearest neighbors to identify.
#' Note that this parameter is not actually used in the index construction itself, and is only included here so that the output index fully parametrizes the search.
#' 
#' Technically, the index construction algorithm is stochastic but, for various logistical reasons, the seed is hard-coded into the C++ code.
#' This means that the results of the HNSW neighbor searches will be fully deterministic for the same inputs, even though the theory provides no such guarantees.
#' 
#' @return 
#' An \linkS4class{AnnoyIndex} object containing a path to the index file, plus additional parameters for the search.
#' 
#' @seealso
#' \linkS4class{HnswIndex}, for details on the output class.
#' 
#' \code{\link{findHnsw}} and \code{\link{queryHnsw}}, for dependent functions.
#' 
#' @author
#' Aaron Lun
#' 
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' out <- buildHnsw(Y)
#' out
#' 
#' @export
#' @importFrom Matrix t
buildHnsw <- function(X, transposed=FALSE, nlinks=16, ef.construction=200, directory=tempdir(), ef.search=10,
    fname=tempfile(tmpdir=directory, fileext=".idx"), distance=c("Euclidean", "Manhattan", "Cosine"))
{
    X <- .coerce_matrix_build(X, transposed)
    distance <- match.arg(distance)
    if (distance=="Cosine") {
        X <- l2norm(X)
    }

    build_hnsw(X, nlinks, ef.construction, fname, distance)
    HnswIndex(data=X, path=fname, ef.search=ef.search, NAMES=colnames(X), distance=distance)
}
