# Tests rangeQueryKmknn().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-range-query-kmknn.R")

set.seed(1001)
test_that("rangeQueryKmknn() behaves correctly with queries", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (d in c(0.1, 0.5, 1)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)
            out <- rangeQueryKmknn(X, threshold=d, query=Y)
            ref <- refQueryNeighbors(X, Y, d)
            expect_identical_re(out, ref)
        }
    }
})

set.seed(1002)
test_that("rangeQueryKmknn() works correctly with subsetting", {
    nobs <- 1000
	nquery <- 93
    ndim <- 21
    d <- 1.5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
	Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- rangeQueryKmknn(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.

    i <- sample(nquery, 20)
    sub <- rangeQueryKmknn(X, Y, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    i <- rbinom(nquery, 1, 0.5) == 0L
    sub <- rangeQueryKmknn(X, Y, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 50)
    sub <- rangeQueryKmknn(X, Y, threshold=d, subset=i)
    m <- match(i, rownames(Y))
    expect_identical_re(sub, lapply(ref, "[", i=m))
})

set.seed(1003)
test_that("rangeQueryKmknn() behaves correctly with alternative options", {
    nobs <- 1000
    nquery <- 100
    ndim <- 10
    d <- 1

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- rangeQueryKmknn(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
    
    # Checking what we extract.
    out2 <- rangeQueryKmknn(X, Y, threshold=d, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(lapply(out2$index, sort), lapply(ref$index, sort))

    out3 <- rangeQueryKmknn(X, Y, threshold=d, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_equal(lapply(out3$distance, sort), lapply(ref$distance, sort))
  
    # Checking precomputation.
    pre <- buildKmknn(X)
    out4 <- rangeQueryKmknn(query=Y, threshold=d, precomputed=pre) # no need for X!
    expect_identical_re(out4, ref)

    # Checking transposition.
    out5 <- rangeQueryKmknn(X, threshold=d, query=t(Y), transposed=TRUE)
    expect_identical_re(out5, ref)
})

set.seed(1003001)
test_that("rangeQueryKmknn() works with Manhattan distances", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10)) {
        for (d in c(0.1, 0.5, 1)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)
            out <- rangeQueryKmknn(X, threshold=d, query=Y, distance="Manhattan")
            ref <- refQueryNeighbors(X, Y, d, type="manhattan")
            expect_identical_re(out, ref)
        }
    }
})

set.seed(10030011)
test_that("rangeQueryKmknn() works with Cosine distances", {
    ndata <- 1000
    nquery <- 100
    ndim <- 10
    d <- 0.2

    X <- matrix(runif(ndata * ndim), nrow=ndata)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)

    out <- rangeQueryKmknn(X, threshold=d, query=Y, distance="Cosine")
    ref <- rangeQueryKmknn(X/sqrt(rowSums(X^2)), threshold=d, query=Y/sqrt(rowSums(Y^2)))
    expect_identical_re(ref, out)
})

set.seed(1003002)
test_that("rangeQueryKmknn() works with counting only", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10)) {
        for (d in c(0.1, 0.5, 1)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            ref <- rangeQueryKmknn(X, threshold=d, query=Y) 
            out <- rangeQueryKmknn(X, threshold=d, query=Y, get.distance=FALSE, get.index=FALSE) 
            expect_identical(out, lengths(ref$index))

            subset <- sample(nquery, 50)
            out.sub <- rangeQueryKmknn(X, subset=subset, threshold=d, query=Y, get.index=FALSE, get.distance=FALSE)
            expect_identical(out[subset], out.sub)
        }
    }
})

set.seed(100301)
test_that("rangeQueryKmknn() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    d <- 1.5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- rangeQueryKmknn(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
  
    # Trying out different types of parallelization.
    out1 <- rangeQueryKmknn(X, Y, threshold=d, BPPARAM=safeBPParam(2))
    expect_identical_re(out1, ref)

    out2 <- rangeQueryKmknn(X, Y, threshold=d, BPPARAM=SnowParam(3))
    expect_identical_re(out2, ref)
})

set.seed(10031)
test_that("rangeQueryKmknn() raw output behaves correctly", {
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    d <- 1.5
    X <- matrix(runif(nobs * ndim), nrow=nobs)
  	Y <- matrix(runif(nquery * ndim), nrow=nquery)
 
    pre <- buildKmknn(X)
    out <- rangeQueryKmknn(query=Y, threshold=d, precomputed=pre, raw.index=TRUE)
    ref <- rangeQueryKmknn(query=Y, X=t(bndata(pre)), threshold=d)
    expect_identical_re(out, ref)

    # Behaves with subsetting.
    i <- sample(nquery, 20)
    out <- rangeQueryKmknn(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeQueryKmknn(query=Y, X=t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    i <- rbinom(nquery, 1, 0.5) == 0L
    out <- rangeQueryKmknn(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeQueryKmknn(query=Y, X=t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    # Adding row names.
    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 30)
    out <- rangeQueryKmknn(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeQueryKmknn(query=Y, X=t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)
})

set.seed(10032)
test_that("rangeQueryKmknn() behaves with variable distances", {
    nobs <- 1021
    nquery <- 121
    ndim <- 8
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)

    available <- c(0.5, 1, 2)
    chosen <- sample(length(available), nquery, replace=TRUE)
    d <- available[chosen]

    out <- rangeQueryKmknn(X, Y, threshold=d)
    for (a in seq_along(available)) {
        current <- chosen==a
        expect_identical_re(lapply(out, "[", i=current), 
            rangeQueryKmknn(X, Y, threshold=available[a], subset=current))
    }

    # Handles subsetting.
    scrambled <- sample(nquery, 40)
    out2 <- rangeQueryKmknn(X, Y, threshold=d[scrambled], subset=scrambled)
    expect_identical_re(out2, lapply(out, "[", i=scrambled))

    scrambled <- rbinom(nquery, 1, 0.5)==1
    out2 <- rangeQueryKmknn(X, Y, threshold=d[scrambled], subset=scrambled)
    expect_identical_re(out2, lapply(out, "[", i=scrambled))

    # Handles parallelization.
    expect_identical_re(out, rangeQueryKmknn(X, Y, threshold=d, BPPARAM=safeBPParam(3)))
})

set.seed(1004)
test_that("rangeQueryKmknn() behaves correctly with silly inputs", {
    nobs <- 1000
	nquery <- 100
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    
    # What happens when the threshold is not positive.
    expect_error(rangeQueryKmknn(X, Y, threshold=0), "positive")
    expect_error(rangeQueryKmknn(X, Y, threshold=-1), "positive")

    # What happens when 'threshold' is not of appropriate length.
    expect_error(rangeQueryKmknn(X, Y, threshold=0:1), "should be equal to")

    # What happens when there are no points in the data.
    out <- rangeQueryKmknn(X[0,,drop=FALSE], Y, threshold=1)
    expect_identical(lengths(out$index), integer(nquery))
    expect_identical(lengths(out$distance), integer(nquery))

    # What happens when there are no points in the query.
    out <- rangeQueryKmknn(X, Y[0,,drop=FALSE], threshold=1)
    expect_identical(out$index, list())
    expect_identical(out$distance, list())

    # What happens when there are no dimensions.
    out <- rangeQueryKmknn(X[,0], Y[,0], threshold=1)
    expect_identical(unique(out$index), list(seq_len(nobs)))
    expect_identical(unique(out$distance), list(numeric(nobs)))

    # What happens when the query is of a different dimension.
    Z <- matrix(runif(nobs * ndim * 2), nrow=nobs)
    expect_error(rangeQueryKmknn(X, threshold=1, query=Z), "dimensionality")

    # What happens when we request raw.index without precomputed.
    expect_error(rangeQueryKmknn(X, Y, threshold=1, raw.index=TRUE), "not valid")

    # What happens when the query is not, strictly a matrix.
    AA <- data.frame(Y)
    colnames(AA) <- NULL
    expect_identical_re(rangeQueryKmknn(X, Y, threshold=1), rangeQueryKmknn(X, AA, threshold=1))
})
