#' @export
#' @importFrom Biocgenerics t
buildHnsw <- function(X, transposed=FALSE, nlinks=16, ef.construction=200, directory=tempdir(), fname=tempfile(tmpdir=directory, fileext=".idx")) 
# Builds an Hnsw index at the specified path.
# 
# written by Aaron Lun
# created 25 September 2018
{
    if (transposed) {
        tX <- X
    } else {
        tX <- t(X)
    }
    if (!is.matrix(tX)) {
        tX <- as.matrix(tX)
    }
    .Call(cxx_build_hnsw, tX, nlinks, ef.construction, fname) 
    HnswIndex(data=tX, path=fname, NAMES=colnames(tX))
}
