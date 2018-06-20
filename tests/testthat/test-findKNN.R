# Tests findKNN().
# library(kmknn); library(testthat); source("test-findKNN.R")

library(FNN)
set.seed(1001)
test_that("findKNN() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- findKNN(X, k=k)
            ref <- get.knn(X, k=k)
            expect_identical(out$index, ref$nn.index)
            expect_equal(out$distance, ref$nn.dist)
        }
    }
})

set.seed(1002)
test_that("findKNN() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    ref <- findKNN(X, k=k)

    i <- sample(nobs, 20)
    sub <- findKNN(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nobs, 1, 0.5) == 0L
    sub <- findKNN(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 100)
    sub <- findKNN(X, k=k, subset=i)
    m <- match(i, rownames(X))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(1003)
test_that("findKNN() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findKNN(X, k=k)
    
    # Checking what we extract.
    out2 <- findKNN(X, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- findKNN(X, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation.
    pre <- precluster(X)
    out4 <- findKNN(X, k=k, precomputed=pre)
    expect_identical(out4, out)
})

set.seed(1004)
test_that("findKNN() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(findKNN(X, k=0), "positive")
    expect_error(findKNN(X, k=-1), "positive")

    # What happens when 'k' > dataset size.
    restrict <- 10
    expect_warning(out <- findKNN(X[seq_len(restrict),], k=20), "capped")
    expect_warning(ref <- findKNN(X[seq_len(restrict),], k=restrict-1L), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- findKNN(X[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nobs))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))
})
