# Tests rangeFindKmknn().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-range-find-kmknn.R")

set.seed(1001)
test_that("rangeFindKmknn() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (d in c(0.1, 0.5, 1)) {
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            ref <- refFindNeighbors(X, d)
            out <- rangeFindKmknn(X, threshold=d)
            expect_identical_re(out, ref)
        }
    }
})

set.seed(1002)
test_that("rangeFindKmknn() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    ref <- rangeFindKmknn(X, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.

    i <- sample(nobs, 20)
    sub <- rangeFindKmknn(X, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    i <- rbinom(nobs, 1, 0.5) == 0L
    sub <- rangeFindKmknn(X, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 123)
    sub <- rangeFindKmknn(X, threshold=d, subset=i)
    m <- match(i, rownames(X))
    expect_identical_re(sub, lapply(ref, "[", i=m))
})

set.seed(1003)
test_that("rangeFindKmknn() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    ref <- rangeFindKmknn(X, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
    
    # Checking what we extract.
    out2 <- rangeFindKmknn(X, threshold=d, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(lapply(out2$index, sort), lapply(ref$index, sort))

    out3 <- rangeFindKmknn(X, threshold=d, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_equal(lapply(out3$distance, sort), lapply(ref$distance, sort))
  
    # Checking precomputation.
    pre <- buildKmknn(X)
    out4 <- rangeFindKmknn(X, threshold=d, precomputed=pre)
    expect_identical_re(out4, ref)
})

set.seed(1003001)
test_that("rangeFindKmknn() works with Manhattan distances", {
    nobs <- 1000
    for (ndim in c(1, 5, 10)) {
        for (d in c(0.1, 0.5, 1)) {
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            ref <- refFindNeighbors(X, d, type="manhattan")
            out <- rangeFindKmknn(X, threshold=d, distance="Manhattan")
            expect_identical_re(out, ref)
        }
    }
})

set.seed(1003002)
test_that("rangeFindKmknn() works with counting only", {
    nobs <- 1000
    for (ndim in c(1, 5, 10)) {
        for (d in c(0.1, 0.5, 1)) {
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            ref <- rangeFindKmknn(X, threshold=d)
            out <- rangeFindKmknn(X, threshold=d, get.index=FALSE, get.distance=FALSE)
            expect_identical(out, lengths(ref$index))

            subset <- sample(nobs, 200)
            out.sub <- rangeFindKmknn(X, subset=subset, threshold=d, get.index=FALSE, get.distance=FALSE)
            expect_identical(out[subset], out.sub)
        }
    }
})

set.seed(100301)
test_that("rangeFindKmknn() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1001
    ndim <- 8
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    ref <- rangeFindKmknn(X, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
  
    # Trying out different types of parallelization.
    out1 <- rangeFindKmknn(X, threshold=d, BPPARAM=safeBPParam(2))
    expect_identical_re(ref, out1)

    out2 <- rangeFindKmknn(X, threshold=d, BPPARAM=SnowParam(3))
    expect_identical_re(ref, out2)
})

set.seed(10031)
test_that("rangeFindKmknn() raw output behaves correctly", {
    nobs <- 1001
    ndim <- 8
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    pre <- buildKmknn(X)
    out <- rangeFindKmknn(threshold=d, precomputed=pre, raw.index=TRUE)
    ref <- rangeFindKmknn(t(bndata(pre)), threshold=d)
    expect_identical_re(out, ref)

    # Behaves with subsetting.
    i <- sample(nobs, 20)
    out <- rangeFindKmknn(threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeFindKmknn(t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    i <- rbinom(nobs, 1, 0.5) == 0L
    out <- rangeFindKmknn(threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeFindKmknn(t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    # Adding row names.
    rownames(X) <- paste0("CELL", seq_len(nobs))
    preN <- buildKmknn(X)
    i <- sample(rownames(X), 30)
    out <- rangeFindKmknn(threshold=d, precomputed=preN, raw.index=TRUE, subset=i)
    ref <- rangeFindKmknn(t(bndata(preN)), threshold=d, subset=i)
    expect_identical_re(out, ref)
})

set.seed(10032)
test_that("rangeFindKmknn() behaves with variable distances", {
    nobs <- 1021
    ndim <- 8
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    available <- c(0.5, 1, 2)
    chosen <- sample(length(available), nobs, replace=TRUE)
    d <- available[chosen]

    out <- rangeFindKmknn(X, threshold=d)
    for (a in seq_along(available)) {
        current <- chosen==a
        expect_identical_re(lapply(out, "[", i=current), 
            rangeFindKmknn(X, threshold=available[a], subset=current))
    }

    # Handles subsetting.
    scrambled <- sample(nobs, 100)
    out2 <- rangeFindKmknn(X, threshold=d[scrambled], subset=scrambled)
    expect_identical_re(out2, lapply(out, "[", i=scrambled))

    scrambled <- rbinom(nobs, 1, 0.5)==1
    out2 <- rangeFindKmknn(X, threshold=d[scrambled], subset=scrambled)
    expect_identical_re(out2, lapply(out, "[", i=scrambled))

    # Handles parallelization.
    expect_identical_re(out, rangeFindKmknn(X, threshold=d, BPPARAM=safeBPParam(3)))
})

set.seed(1004)
test_that("rangeFindKmknn() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(rangeFindKmknn(X, threshold=0), "positive")
    expect_error(rangeFindKmknn(X, threshold=-1), "positive")

    # What happens when there are no points.
    out <- rangeFindKmknn(X[0,], threshold=1)
    expect_equal(out$index, list())
    expect_equal(out$distance, list())

    # What happens when 'threshold' is not of appropriate length.
    expect_error(rangeFindKmknn(X, threshold=0:1), "should be equal to")

    # What happens when there are no dimensions.
    out <- rangeFindKmknn(X[,0], threshold=1)
    expect_identical(unique(out$index), list(seq_len(nobs)))
    expect_identical(unique(out$distance), list(numeric(nobs)))
    
    # What happens when we request raw.index without precomputed.
    expect_error(rangeFindKmknn(X, threshold=1, raw.index=TRUE), "not valid")
})
