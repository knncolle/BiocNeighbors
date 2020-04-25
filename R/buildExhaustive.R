#' @export
#' @importFrom Matrix t
buildExhaustive <- function(X, transposed=FALSE, distance=c("Euclidean", "Manhattan"))
# Builds an Exhaustive index.
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

    ExhaustiveIndex(data=tX, NAMES=colnames(tX), distance=distance)
}
