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

    out <- .Call(cxx_build_vptree, t(X))
    colnames(out[[1]]) <- rownames(X)[out[[2]]] # for consistency when subsetting raw indices.
    VptreeIndex(data=out[[1]], order=out[[2]], nodes=out[-(1:2)], NAMES=rownames(X))
}
