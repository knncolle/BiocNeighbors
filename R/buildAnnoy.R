#' @export
buildAnnoy <- function(X, ntrees=50, directory=tempdir(), fname=tempfile(tmpdir=directory, fileext=".idx")) 
# Builds an Annoy index at the specified path.
# 
# written by Aaron Lun
# created 25 September 2018
{
    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }
    .Call(cxx_build_annoy, t(X), ntrees, fname) 
    AnnoyIndex(path=fname, dim=dim(X), NAMES=rownames(X))
}
