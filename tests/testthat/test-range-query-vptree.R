# Tests rangeQueryVptree().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-range-query-vptree.R")

set.seed(1001)
test_that("rangeQueryVptree() behaves correctly with queries", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (d in c(0.1, 0.5, 1)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)
            out <- rangeQueryVptree(X, threshold=d, query=Y)

            out.dist <- out.indx <- vector("list", nquery)
            for (j in seq_len(nquery)) {
                targets <- sqrt(colSums((Y[j,] - t(X))^2))
                chosen <- targets <= d
                out.indx[[j]] <- which(chosen)
                out.dist[[j]] <- targets[chosen]
            }

            expect_identical_re(out, list(index=out.indx, distance=out.dist))
        }
    }
})

set.seed(1002)
test_that("rangeQueryVptree() works correctly with subsetting", {
    nobs <- 1000
	nquery <- 93
    ndim <- 21
    d <- 1.5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
	Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- rangeQueryVptree(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.

    i <- sample(nquery, 20)
    sub <- rangeQueryVptree(X, Y, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    i <- rbinom(nquery, 1, 0.5) == 0L
    sub <- rangeQueryVptree(X, Y, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 50)
    sub <- rangeQueryVptree(X, Y, threshold=d, subset=i)
    m <- match(i, rownames(Y))
    expect_identical_re(sub, lapply(ref, "[", i=m))
})

set.seed(1003)
test_that("rangeQueryVptree() behaves correctly with alternative options", {
    nobs <- 1000
    nquery <- 100
    ndim <- 10
    d <- 1

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- rangeQueryVptree(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
    
    # Checking what we extract.
    out2 <- rangeQueryVptree(X, Y, threshold=d, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(lapply(out2$index, sort), lapply(ref$index, sort))

    out3 <- rangeQueryVptree(X, Y, threshold=d, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_equal(lapply(out3$distance, sort), lapply(ref$distance, sort))
  
    # Checking precomputation.
    pre <- buildVptree(X)
    out4 <- rangeQueryVptree(query=Y, threshold=d, precomputed=pre) # no need for X!
    expect_identical_re(out4, ref)

    # Checking transposition.
    out5 <- rangeQueryVptree(X, threshold=d, query=t(Y), transposed=TRUE)
    expect_identical_re(out5, ref)
})

set.seed(100301)
test_that("rangeQueryVptree() behaves correctly with parallelization", {
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    d <- 1.5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- rangeQueryVptree(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
  
    # Trying out different types of parallelization.
    out1 <- rangeQueryVptree(X, Y, threshold=d, BPPARAM=MulticoreParam(2))
    expect_identical_re(out1, ref)

    out2 <- rangeQueryVptree(X, Y, threshold=d, BPPARAM=SnowParam(3))
    expect_identical_re(out2, ref)
})

set.seed(10031)
test_that("rangeQueryVptree() raw output behaves correctly", {
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    d <- 1.5
    X <- matrix(runif(nobs * ndim), nrow=nobs)
  	Y <- matrix(runif(nquery * ndim), nrow=nquery)
 
    pre <- buildVptree(X)
    out <- rangeQueryVptree(query=Y, threshold=d, precomputed=pre, raw.index=TRUE)
    ref <- rangeQueryVptree(query=Y, X=t(bndata(pre)), threshold=d)
    expect_identical_re(out, ref)

    # Behaves with subsetting.
    i <- sample(nquery, 20)
    out <- rangeQueryVptree(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeQueryVptree(query=Y, X=t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    i <- rbinom(nquery, 1, 0.5) == 0L
    out <- rangeQueryVptree(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeQueryVptree(query=Y, X=t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    # Adding row names.
    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 30)
    out <- rangeQueryVptree(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- rangeQueryVptree(query=Y, X=t(bndata(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)
})

set.seed(10032)
test_that("rangeQueryVptree() behaves with variable distances", {
    nobs <- 1021
    nquery <- 121
    ndim <- 8
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)

    available <- c(0.5, 1, 2)
    chosen <- sample(length(available), nquery, replace=TRUE)
    d <- available[chosen]

    out <- rangeQueryVptree(X, Y, threshold=d)
    for (a in seq_along(available)) {
        current <- chosen==a
        expect_identical_re(lapply(out, "[", i=current), 
            rangeQueryVptree(X, Y, threshold=available[a], subset=current))
    }

    # Handles subsetting.
    scrambled <- sample(nquery, 40)
    out2 <- rangeQueryVptree(X, Y, threshold=d[scrambled], subset=scrambled)
    expect_identical_re(out2, lapply(out, "[", i=scrambled))

    scrambled <- rbinom(nquery, 1, 0.5)==1
    out2 <- rangeQueryVptree(X, Y, threshold=d[scrambled], subset=scrambled)
    expect_identical_re(out2, lapply(out, "[", i=scrambled))

    # Handles parallelization.
    expect_identical_re(out, rangeQueryVptree(X, Y, threshold=d, BPPARAM=MulticoreParam(3)))
})

set.seed(1004)
test_that("rangeQueryVptree() behaves correctly with silly inputs", {
    nobs <- 1000
	nquery <- 100
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    
    # What happens when the threshold is not positive.
    expect_error(rangeQueryVptree(X, Y, threshold=0), "positive")
    expect_error(rangeQueryVptree(X, Y, threshold=-1), "positive")

    # What happens when 'threshold' is not of appropriate length.
    expect_error(rangeQueryVptree(X, Y, threshold=0:1), "should be equal to")

    # What happens when there are no points in the data.
    out <- rangeQueryVptree(X[0,,drop=FALSE], Y, threshold=1)
    expect_identical(lengths(out$index), integer(nquery))
    expect_identical(lengths(out$distance), integer(nquery))

    # What happens when there are no points in the query.
    out <- rangeQueryVptree(X, Y[0,,drop=FALSE], threshold=1)
    expect_identical(out$index, list())
    expect_identical(out$distance, list())

    # What happens when there are no dimensions.
    out <- rangeQueryVptree(X[,0], Y[,0], threshold=1)
    U <- unique(out$index)
    expect_identical(length(U), 1L)
    expect_identical(sort(U[[1]]), seq_len(nobs))
    expect_identical(unique(out$distance), list(numeric(nobs)))

    # What happens when the query is of a different dimension.
    Z <- matrix(runif(nobs * ndim * 2), nrow=nobs)
    expect_error(rangeQueryVptree(X, threshold=1, query=Z), "dimensionality")

    # What happens when we request raw.index without precomputed.
    expect_error(rangeQueryVptree(X, Y, threshold=1, raw.index=TRUE), "not valid")

    # What happens when the query is not, strictly a matrix.
    AA <- data.frame(Y)
    colnames(AA) <- NULL
    expect_identical_re(rangeQueryVptree(X, Y, threshold=1), rangeQueryVptree(X, AA, threshold=1))
})
