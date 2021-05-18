# Tests findExhaustive().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-find-exhaustive.R")

set.seed(1001)
test_that("findExhaustive() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- findExhaustive(X, k=k)
            ref <- refFindKNN(X, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance)
        }
    }
})

set.seed(1002)
test_that("findExhaustive() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    ref <- findExhaustive(X, k=k)

    i <- sample(nobs, 20)
    sub <- findExhaustive(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nobs, 1, 0.5) == 0L
    sub <- findExhaustive(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 100)
    sub <- findExhaustive(X, k=k, subset=i)
    m <- match(i, rownames(X))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(1003)
test_that("findExhaustive() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findExhaustive(X, k=k)
    
    # Checking what we extract.
    out2 <- findExhaustive(X, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- findExhaustive(X, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation (does not need X).
    pre <- buildExhaustive(X)
    out4 <- findExhaustive(k=k, precomputed=pre)
    expect_identical(out4, out)
})

set.seed(1003001)
test_that("findExhaustive() behaves correctly with Manhattan distances", {
    nobs <- 500 # fewer observations, as refFindKNN is a slow brute-force method.
    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- findExhaustive(X, k=k, distance="Manhattan")
            ref <- refFindKNN(X, k=k, type="manhattan")
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance)
        }
    }
})

set.seed(1003001)
test_that("findExhaustive() behaves correctly with Cosine distances", {
    # No need for fancy stuff here.
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findExhaustive(X, k=5, distance="Cosine")
    ref <- findExhaustive(X / sqrt(rowSums(X^2)), k=5)
    expect_identical(out, ref)
})

set.seed(1003002)
test_that("findExhaustive() behaves correctly when only the last distance is requested", {
    nobs <- 500 # fewer observations, as refFindKNN is a slow brute-force method.
    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)

            ref <- findExhaustive(X, k=k)
            out <- findExhaustive(X, k=k, last=1)
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])

            ref <- findExhaustive(X, k=k, distance="Manhattan")
            out <- findExhaustive(X, k=k, last=1, distance="Manhattan")
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])
        }
    }
})

set.seed(100301)
test_that("findExhaustive() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findExhaustive(X, k=k)
  
    # Trying out different types of parallelization.
    out1 <- findExhaustive(X, k=k, BPPARAM=safeBPParam(2))
    expect_identical(out$index, out1$index)
    expect_identical(out$distance, out1$distance)

    out2 <- findExhaustive(X, k=k, BPPARAM=SnowParam(3))
    expect_identical(out$index, out2$index)
    expect_identical(out$distance, out2$distance)
})

set.seed(10031)
test_that("findExhaustive() raw output behaves correctly", {
    nobs <- 1000
    ndim <- 10
    k <- 7
    X <- matrix(runif(nobs * ndim), nrow=nobs)
   
    pre <- buildExhaustive(X)
    out <- findExhaustive(k=k, precomputed=pre, raw.index=TRUE)
    ref <- findExhaustive(t(bndata(pre)), k=k)
    expect_identical(out, ref)

    # Behaves with subsetting.
    i <- sample(nobs, 20)
    out <- findExhaustive(k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- findExhaustive(t(bndata(pre)), k=k, subset=i)
    expect_identical(out, ref)

    i <- rbinom(nobs, 1, 0.5) == 0L
    out <- findExhaustive(k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- findExhaustive(t(bndata(pre)), k=k, subset=i)
    expect_identical(out, ref)

    # Adding row names.
    rownames(X) <- paste0("CELL", seq_len(nobs))
    preN <- buildExhaustive(X)
    i <- sample(rownames(X), 30)
    out <- findExhaustive(k=k, precomputed=preN, raw.index=TRUE, subset=i)
    ref <- findExhaustive(t(bndata(preN)), k=k, subset=i)
    expect_identical(out, ref)
})

set.seed(1004)
test_that("findExhaustive() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(findExhaustive(X, k=0), "positive")
    expect_error(findExhaustive(X, k=-1), "positive")

    # What happens when 'k' > dataset size.
    restrict <- 10
    expect_warning(out <- findExhaustive(X[seq_len(restrict),], k=20), "capped")
    expect_warning(ref <- findExhaustive(X[seq_len(restrict),], k=restrict-1L), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- findExhaustive(X[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nobs))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens when we request raw.index without precomputed.
    expect_error(findExhaustive(X, k=20, raw.index=TRUE), "not valid")

    # What happens with nothing.
    expect_identical(findExhaustive(X, k=10, get.distance=FALSE, get.index=FALSE), list())
})
