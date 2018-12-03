#' @export
buildVptree <- function(X)
# Builds an VP tree index.
# 
# written by Aaron Lun
# created 2 December 2018
{
    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }

    tX <- t(X)
    out <- .Call(cxx_build_vptree, tX)
    ordering <- out[[1]]
    VptreeIndex(data=tX[,ordering,drop=FALSE], order=ordering, nodes=out[-1], NAMES=rownames(X))
}
