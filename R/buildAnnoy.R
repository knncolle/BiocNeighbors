#' @export
#' @importFrom BiocGenerics t
buildAnnoy <- function(X, transposed=FALSE, ntrees=50, directory=tempdir(), fname=tempfile(tmpdir=directory, fileext=".idx")) 
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
    .Call(cxx_build_annoy, tX, ntrees, fname) 
    AnnoyIndex(data=tX, path=fname, NAMES=colnames(tX))
}
