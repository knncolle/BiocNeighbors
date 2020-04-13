# Tests queryAnnoy().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-query-annoy.R")

skip_on_os("windows") # ??? Who knows. 32-bit, huh.

set.seed(1001)
test_that("queryAnnoy() behaves correctly with queries", {
    library(RcppAnnoy)
    REFFUN <- function(X, Y, k, ntrees=50) {
        a <- new(AnnoyEuclidean, ncol(X))
        for (i in seq_len(nrow(X))) {
            a$addItem(i-1L, X[i,])
        }
        a$build(ntrees)
    
        collected.dex <- collected.dist <- vector("list", nrow(X))
        for (i in seq_len(nrow(Y))) {
            available <- a$getNNsByVector(Y[i,], k) + 1L
            available <- head(available, k)
            collected.dex[[i]] <- available
            collected.dist[[i]] <- sqrt(colSums((Y[i,] - t(X[available,,drop=FALSE]))^2))
        }
    
        list(index=do.call(rbind, collected.dex),
            distance=do.call(rbind, collected.dist))            
    }

    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            out <- queryAnnoy(X, k=k, query=Y)
            ref <- REFFUN(X, Y, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance, tol=1e-6) # imprecision due to RcppAnnoy's use of floats.
        }
    }
})

set.seed(1002)
test_that("queryAnnoy() works correctly with subsetting", {
    nobs <- 1000
	nquery <- 93
    ndim <- 21
    k <- 7

    X <- matrix(runif(nobs * ndim), nrow=nobs)
	Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- queryAnnoy(X, Y, k=k)

    i <- sample(nquery, 20)
    sub <- queryAnnoy(X, Y, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nquery, 1, 0.5) == 0L
    sub <- queryAnnoy(X, Y, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 50)
    sub <- queryAnnoy(X, Y, k=k, subset=i)
    m <- match(i, rownames(Y))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(1003)
test_that("queryAnnoy() behaves correctly with alternative options", {
    nobs <- 1000
    nquery <- 100
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    out <- queryAnnoy(X, Y, k=k)
    
    # Checking what we extract.
    out2 <- queryAnnoy(X, Y, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- queryAnnoy(X, Y, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation.
    pre <- buildAnnoy(X)
    out4 <- queryAnnoy(query=Y, k=k, precomputed=pre) # no need for X!
    expect_identical(out4, out)

    # Checking transposition.
    out5 <- queryAnnoy(X, k=k, query=t(Y), transposed=TRUE)
    expect_identical(out5, out)
})

set.seed(1003001)
test_that("queryAnnoy() behaves correctly with Manhattan distances", {
    REFFUN <- function(X, Y, k, ntrees=50) {
        a <- new(AnnoyManhattan, ncol(X))
        for (i in seq_len(nrow(X))) {
            a$addItem(i-1L, X[i,])
        }
        a$build(ntrees)

        collected.dex <- collected.dist <- vector("list", nrow(X))
        for (i in seq_len(nrow(Y))) {
            available <- a$getNNsByVector(Y[i,], k) + 1L
            available <- head(available, k)
            collected.dex[[i]] <- available
            collected.dist[[i]] <- colSums(abs(Y[i,] - t(X[available,,drop=FALSE])))
        }

        list(index=do.call(rbind, collected.dex),
            distance=do.call(rbind, collected.dist))            
    }

    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            out <- queryAnnoy(X, k=k, query=Y, distance="Manhattan")
            ref <- REFFUN(X, Y, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance, tol=1e-6) # imprecision due to RcppAnnoy's use of floats.
        }
    }
})

set.seed(1003001)
test_that("queryAnnoy() works to only obtain the last distance", {
    ndata <- 500 
    nquery <- 100

    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) {
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            ref <- queryAnnoy(X, k=k, query=Y)
            out <- queryAnnoy(X, k=k, query=Y, last=1)
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])

            ref <- queryAnnoy(X, k=k, query=Y, distance="Manhattan")
            out <- queryAnnoy(X, k=k, query=Y, last=1, distance="Manhattan")
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])
        }
    }
})

set.seed(1003002)
test_that("queryAnnoy() responds to run-time search.k", {
    nobs <- 1000
    nquery <- 100
    ndim <- 12
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)

    k <- 7
    ref <- queryAnnoy(X, Y, k=k)
    alt <- queryAnnoy(X, Y, k=k, search.mult=20)
    expect_false(identical(alt$index, ref$index))
    
    # As a control:
    alt <- queryAnnoy(X, Y, k=k, search.mult=50)
    expect_true(identical(alt$index, ref$index))
})

set.seed(100301)
test_that("queryAnnoy() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1000
    nquery <- 124
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    out <- queryAnnoy(X, Y, k=k)
  
    # Trying out different types of parallelization.
    out1 <- queryAnnoy(X, Y, k=k, BPPARAM=safeBPParam(2))
    expect_identical(out$index, out1$index)
    expect_identical(out$distance, out1$distance)

    out2 <- queryAnnoy(X, Y, k=k, BPPARAM=SnowParam(3))
    expect_identical(out$index, out2$index)
    expect_identical(out$distance, out2$distance)
})

set.seed(1004)
test_that("queryAnnoy() behaves correctly with silly inputs", {
    nobs <- 1000
	nquery <- 100
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    
    # What happens when k is not positive.
    expect_error(queryAnnoy(X, Y, k=0), "positive")
    expect_error(queryAnnoy(X, Y, k=-1), "positive")

    # What happens when there are more NNs than k.
    restrict <- 10
    expect_warning(out <- queryAnnoy(X[seq_len(restrict),], Y, k=20), "capped")
    expect_warning(ref <- queryAnnoy(X[seq_len(restrict),], Y, k=restrict), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- queryAnnoy(X[,0], Y[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nquery))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens when the query is of a different dimension.
    Z <- matrix(runif(nobs * ndim * 2), nrow=nobs)
    expect_error(queryAnnoy(X, k=20, query=Z), "dimensionality")

    # What happens when the query is not, strictly a matrix.
    AA <- data.frame(Y)
    colnames(AA) <- NULL
    expect_equal(queryAnnoy(X, Y, k=20), queryAnnoy(X, AA, k=20))

    # What happens with nothing.
    expect_identical(queryAnnoy(X, Y, k=10, get.distance=FALSE, get.index=FALSE), list())
})
