# Tests findNeighbors().
# library(kmknn); library(testthat); source("test-findNeighbors.R")

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

set.seed(1001)
test_that("findNeighbors() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (d in c(0.1, 0.5, 1)) {
            X <- matrix(runif(nobs * ndim), nrow=nobs)
    
            D <- unname(as.matrix(dist(X)))
            ind <- which(D <= d, arr.ind=TRUE)
            by.row <- split(ind[,2], ind[,1])
            by.dist <- split(D[ind], ind[,1])
            ref <- list(index=unname(by.row), distance=unname(by.dist))

            out <- findNeighbors(X, threshold=d)
            expect_identical_re(out, ref)
        }
    }
})

set.seed(1002)
test_that("findNeighbors() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    ref <- findNeighbors(X, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.

    i <- sample(nobs, 20)
    sub <- findNeighbors(X, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    i <- rbinom(nobs, 1, 0.5) == 0L
    sub <- findNeighbors(X, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 123)
    sub <- findNeighbors(X, threshold=d, subset=i)
    m <- match(i, rownames(X))
    expect_identical_re(sub, lapply(ref, "[", i=m))
})

set.seed(1003)
test_that("findNeighbors() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    ref <- findNeighbors(X, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
    
    # Checking what we extract.
    out2 <- findNeighbors(X, threshold=d, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(lapply(out2$index, sort), lapply(ref$index, sort))

    out3 <- findNeighbors(X, threshold=d, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_equal(lapply(out3$distance, sort), lapply(ref$distance, sort))
  
    # Checking precomputation.
    pre <- precluster(X)
    out4 <- findNeighbors(X, threshold=d, precomputed=pre)
    expect_identical_re(out4, ref)
})

set.seed(10031)
test_that("findNeighbors() raw output behaves correctly", {
library(kmknn); library(testthat)
    nobs <- 1001
    ndim <- 8
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    pre <- precluster(X)
    out <- findNeighbors(threshold=d, precomputed=pre, raw.index=TRUE)
    ref <- findNeighbors(t(pre$data), threshold=d)
    expect_identical_re(out, ref)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.

    # Behaves with subsetting.
    i <- sample(nobs, 20)
    out <- findNeighbors(threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- findNeighbors(t(pre$data), threshold=d, subset=i)
    expect_identical_re(out, ref)

    i <- rbinom(nobs, 1, 0.5) == 0L
    out <- findNeighbors(threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- findNeighbors(t(pre$data), threshold=d, subset=i)
    expect_identical_re(out, ref)

    # Adding row names.
    rownames(X) <- paste0("CELL", seq_len(nobs))
    preN <- precluster(X)
    i <- sample(rownames(X), 30)
    out <- findNeighbors(threshold=d, precomputed=preN, raw.index=TRUE, subset=i)
    ref <- findNeighbors(t(preN$data), threshold=d, subset=i)
    expect_identical_re(out, ref)
})

set.seed(1004)
test_that("findNeighbors() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(findNeighbors(X, threshold=0), "positive")
    expect_error(findNeighbors(X, threshold=-1), "positive")

    # What happens when there are no points.
    out <- findNeighbors(X[0,], threshold=1)
    expect_equal(out$index, list())
    expect_equal(out$distance, list())

    # What happens when there are no dimensions.
    out <- findNeighbors(X[,0], threshold=1)
    expect_identical(unique(out$index), list(seq_len(nobs)))
    expect_identical(unique(out$distance), list(numeric(nobs)))
    
    # What happens when we request raw.index without precomputed.
    expect_error(findNeighbors(X, threshold=1, raw.index=TRUE), "not valid")
})
