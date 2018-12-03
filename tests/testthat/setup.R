# Setting up some common functions for range find checks.

REINFORCE <- function(out) {
# Remember that the output indices are unordered, though the identities are constant.
# Thus, we need to do some work to ensure that we get the same result.
    O <- lapply(out$index, order)
    re.index <- mapply(FUN="[", x=out$index, i=O, SIMPLIFY=FALSE)
    re.dist <- mapply(FUN="[", x=out$distance, i=O, SIMPLIFY=FALSE)
    list(index=re.index, distance=re.dist)
}

expect_identical_re <- function(left, right) {
    expect_false(is.null(left$index))
    expect_false(is.null(right$index))
    expect_false(is.null(left$distance))
    expect_false(is.null(right$distance))

    L <- REINFORCE(left)
    R <- REINFORCE(right)
    expect_identical(L$index, R$index)
    expect_equal(L$distance, R$distance)
}


