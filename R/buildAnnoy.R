#' @export
#' @importFrom BiocGenerics t
buildAnnoy <- function(X, transposed=FALSE, ntrees=50, directory=tempdir(), search.mult=ntrees, 
    fname=tempfile(tmpdir=directory, fileext=".idx"), distance=c("Euclidean", "Manhattan")) 
# Builds an Annoy index at the specified path.
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

    .Call(cxx_build_annoy, tX, ntrees, fname, distance)
    AnnoyIndex(data=tX, path=fname, search.mult=search.mult, NAMES=colnames(tX), distance=distance)
}
