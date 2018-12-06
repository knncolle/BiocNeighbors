#' @export
#' @importFrom BiocGenerics t
buildVptree <- function(X, transposed=FALSE)
# Builds an VP tree index.
# 
# written by Aaron Lun
# created 2 December 2018
{
    if (transposed) {
        tX <- X
    } else {
        tX <- t(X)
    }
    if (!is.matrix(tX)) {
        tX <- as.matrix(tX)
    }

    out <- .Call(cxx_build_vptree, tX)
    ordering <- out[[1]]
    VptreeIndex(data=tX[,ordering,drop=FALSE], order=ordering, nodes=out[-1], NAMES=colnames(tX))
}
