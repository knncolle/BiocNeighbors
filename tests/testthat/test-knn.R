# Tests find.knn().
# library(kmknn); library(testthat); source("test-knn.R")

library(FNN)
test_that("find.knn() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- find.knn(X, k=k)
            ref <- get.knn(X, k=k)
            expect_identical(out$index, ref$nn.index)
            expect_equal(out$distance, ref$nn.dist)
        }
    }
})

test_that("find.knn() behaves correctly with queries", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            out <- find.knn(X, k=k, query=Y)
            ref <- get.knnx(data=X, query=Y, k=k)
            expect_identical(out$index, ref$nn.index)
            expect_equal(out$distance, ref$nn.dist)
        }
    }
})

test_that("find.knn() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- find.knn(X, k=k)
    
    # Checking what we extract.
    out2 <- find.knn(X, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- find.knn(X, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation.
    pre <- precluster(X)
    out4 <- find.knn(k=k, precomputed=pre) # works without X at all!
    expect_identical(out4, out)

    # Checking query transposition.
    nquery <- 100
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    outQ <- find.knn(X, k=k, query=Y)
    outQ2 <- find.knn(X, k=k, query=t(Y), query.transposed=TRUE)
    expect_identical(outQ, outQ2)
})

test_that("find.knn() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(find.knn(X, k=0), "positive")
    expect_error(find.knn(X, k=-1), "positive")

    # What happens when there are more NNs than k.
    restrict <- 10
    expect_warning(out <- find.knn(X[seq_len(restrict),], k=20), "capped")
    expect_warning(ref <- find.knn(X[seq_len(restrict),], k=restrict-1L), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- find.knn(X[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nobs))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens when the query is of a different dimension.
    Y <- matrix(runif(nobs * ndim*2), nrow=nobs)
    expect_error(find.knn(X, k=20, query=Y), "dimensionality")
})
