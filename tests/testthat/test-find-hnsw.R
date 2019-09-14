# Tests findHnsw().
# library(BiocNeighbors); library(testthat); source("test-find-hnsw.R")

library(RcppHNSW)
REFFUN <- function(X, k, M=16, ef_construction=200, ef_search=10) {
    out <- RcppHNSW::hnsw_knn(X, k = k+1, distance = "euclidean", M=M, ef_construction = ef_construction, ef=ef_search)
    list(index=out$idx[,-1,drop=FALSE], distance=out$dist[,-1,drop=FALSE])
}

set.seed(7001)
test_that("findHnsw() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- findHnsw(X, k=k)
            ref <- REFFUN(X, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance) # imprecision due to differences between R and C++'s sqrt()?
        }
    }
})

set.seed(7002)
test_that("findHnsw() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    ref <- findHnsw(X, k=k)

    i <- sample(nobs, 20)
    sub <- findHnsw(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nobs, 1, 0.5) == 0L
    sub <- findHnsw(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 100)
    sub <- findHnsw(X, k=k, subset=i)
    m <- match(i, rownames(X))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(7003)
test_that("findHnsw() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findHnsw(X, k=k)
    
    # Checking what we extract.
    out2 <- findHnsw(X, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- findHnsw(X, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation (does not need X).
    pre <- buildHnsw(X)
    out4 <- findHnsw(k=k, precomputed=pre)
    expect_identical(out4, out)
})

set.seed(70031)
test_that("findHnsw() works with Manhattan distances", {
    nobs <- 1000
    ndim <- 10
    k <- 5
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # Can't compare directly as L1Space doesn't exist in RcppHNSW.
    # We just check that the distance calculation is about-right.
    out <- findHnsw(X, k=k, distance="Manhattan")
    for (i in seq_len(k)) {
        val <- rowSums(abs(X - X[out$index[,i],,drop=FALSE]))
        expect_equal(out$distance[,i], val, tol=1e-6)
    }
})

set.seed(700311)
test_that("findHnsw() behaves correctly when only the last distance is requested", {
    nobs <- 500 
    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)

            ref <- findHnsw(X, k=k)
            out <- findHnsw(X, k=k, last=1)
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])

            ref <- findHnsw(X, k=k, distance="Manhattan")
            out <- findHnsw(X, k=k, last=1, distance="Manhattan")
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])
        }
    }
})

set.seed(70032)
test_that("findHnsw() responds to run-time 'ef.search'", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    k <- 7
    ref <- findHnsw(X, k=k)
    alt <- findHnsw(X, k=k, ef.search=20)
    expect_false(identical(alt$index, ref$index))

    # As a control:
    alt <- findHnsw(X, k=k, ef.search=10)
    expect_true(identical(alt$index, ref$index))
})

set.seed(7004)
test_that("findHnsw() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findHnsw(X, k=k)
  
    # Trying out different types of parallelization.
    out1 <- findHnsw(X, k=k, BPPARAM=safeBPParam(2))
    expect_identical(out$index, out1$index)
    expect_identical(out$distance, out1$distance)

    out2 <- findHnsw(X, k=k, BPPARAM=SnowParam(3))
    expect_identical(out$index, out2$index)
    expect_identical(out$distance, out2$distance)
})

set.seed(7005)
test_that("findHnsw() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(findHnsw(X, k=0), "positive")
    expect_error(findHnsw(X, k=-1), "positive")

    # What happens when 'k' > dataset size.
    restrict <- 10
    expect_warning(out <- findHnsw(X[seq_len(restrict),], k=20), "capped")
    expect_warning(ref <- findHnsw(X[seq_len(restrict),], k=restrict-1L), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- findHnsw(X[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nobs))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens with nothing.
    expect_identical(findHnsw(X, k=10, get.distance=FALSE, get.index=FALSE), list())
})
