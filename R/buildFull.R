#' @export
#' @importFrom Matrix t
buildFull <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan"))
# Builds an Full tree index.
# 
# created 19 April 2020
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

    FullIndex(data=tX, order=seq_len(ncol(tX)), NAMES=colnames(tX), distance=distance)
}
