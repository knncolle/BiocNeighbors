#' @export
#' @importFrom BiocGenerics t
buildHnsw <- function(X, transposed=FALSE, nlinks=16, ef.construction=200, directory=tempdir(), ef.search=ef.construction,
    fname=tempfile(tmpdir=directory, fileext=".idx"), distance=c("Euclidean", "Manhattan"))
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
    distance <- match.arg(distance)

    .Call(cxx_build_hnsw, tX, nlinks, ef.construction, fname, distance)
    HnswIndex(data=tX, path=fname, ef.search=ef.search, NAMES=colnames(tX), distance=distance)
}
