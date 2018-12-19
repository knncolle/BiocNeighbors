# Tests queryHnsw().
# library(BiocNeighbors); library(testthat); source("test-query-hnsw.R")

library(RcppHNSW)
REFFUN <- function(X, Y, k, M=16, ef_construction = 200, ef=10) {
    ann <- new(HnswL2, ncol(X), nrow(X), as.integer(M), as.integer(ef_construction))
    for (i in seq_len(nrow(X))) {
        ann$addItem(X[i,])
    }

    ann$setEf(ef)
    collected.dex <- collected.dist <- vector("list", nrow(X))
    for (i in seq_len(nrow(Y))) {
        available <- ann$getNNs(Y[i,], k)
        available <- head(available, k)
        collected.dex[[i]] <- as.integer(available)
        collected.dist[[i]] <- sqrt(colSums((Y[i,] - t(X[available,,drop=FALSE]))^2))
    }

    list(index=do.call(rbind, collected.dex),
        distance=do.call(rbind, collected.dist))            
}

set.seed(1001)
test_that("queryHnsw() behaves correctly with queries", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            out <- queryHnsw(X, k=k, query=Y)
            ref <- REFFUN(X, Y, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance, tol=1e-6) # imprecision due to RcppHNSW's use of floats.
        }
    }
})

set.seed(1002)
test_that("queryHnsw() works correctly with subsetting", {
    nobs <- 1000
	nquery <- 93
    ndim <- 21
    k <- 7

    X <- matrix(runif(nobs * ndim), nrow=nobs)
	Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- queryHnsw(X, Y, k=k)

    i <- sample(nquery, 20)
    sub <- queryHnsw(X, Y, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nquery, 1, 0.5) == 0L
    sub <- queryHnsw(X, Y, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 50)
    sub <- queryHnsw(X, Y, k=k, subset=i)
    m <- match(i, rownames(Y))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(1003)
test_that("queryHnsw() behaves correctly with alternative options", {
    nobs <- 1000
    nquery <- 100
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    out <- queryHnsw(X, Y, k=k)
    
    # Checking what we extract.
    out2 <- queryHnsw(X, Y, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- queryHnsw(X, Y, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation.
    pre <- buildHnsw(X)
    out4 <- queryHnsw(query=Y, k=k, precomputed=pre) # no need for X!
    expect_identical(out4, out)

    # Checking transposition.
    out5 <- queryHnsw(X, k=k, query=t(Y), transposed=TRUE)
    expect_identical(out5, out)
})

set.seed(100301)
test_that("queryHnsw() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1000
    nquery <- 124
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    out <- queryHnsw(X, Y, k=k)
  
    # Trying out different types of parallelization.
    out1 <- queryHnsw(X, Y, k=k, BPPARAM=MulticoreParam(2))
    expect_identical(out$index, out1$index)
    expect_identical(out$distance, out1$distance)

    out2 <- queryHnsw(X, Y, k=k, BPPARAM=SnowParam(3))
    expect_identical(out$index, out2$index)
    expect_identical(out$distance, out2$distance)
})

set.seed(1004)
test_that("queryHnsw() behaves correctly with silly inputs", {
    nobs <- 1000
	nquery <- 100
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    
    # What happens when k is not positive.
    expect_error(queryHnsw(X, Y, k=0), "positive")
    expect_error(queryHnsw(X, Y, k=-1), "positive")

    # What happens when there are more NNs than k.
    restrict <- 10
    expect_warning(out <- queryHnsw(X[seq_len(restrict),], Y, k=20), "capped")
    expect_warning(ref <- queryHnsw(X[seq_len(restrict),], Y, k=restrict), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- queryHnsw(X[,0], Y[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nquery))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens when the query is of a different dimension.
    Z <- matrix(runif(nobs * ndim * 2), nrow=nobs)
    expect_error(queryHnsw(X, k=20, query=Z), "dimensionality")

    # What happens when the query is not, strictly a matrix.
    AA <- data.frame(Y)
    colnames(AA) <- NULL
    expect_equal(queryHnsw(X, Y, k=20), queryHnsw(X, AA, k=20))
})
