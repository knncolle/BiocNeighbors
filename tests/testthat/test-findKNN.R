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
  
    # Checking precomputation (does not need X).
    pre <- precluster(X)
    out4 <- findKNN(k=k, precomputed=pre)
    expect_identical(out4, out)
})

set.seed(10031)
test_that("findKNN() raw output behaves correctly", {
    nobs <- 1000
    ndim <- 10
    k <- 7
    X <- matrix(runif(nobs * ndim), nrow=nobs)
   
    pre <- precluster(X)
    out <- findKNN(k=k, precomputed=pre, raw.index=TRUE)
    ref <- findKNN(t(pre$data), k=k)
    expect_identical(out, ref)

    # Behaves with subsetting.
    i <- sample(nobs, 20)
    out <- findKNN(k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- findKNN(t(pre$data), k=k, subset=i)
    expect_identical(out, ref)

    i <- rbinom(nobs, 1, 0.5) == 0L
    out <- findKNN(k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- findKNN(t(pre$data), k=k, subset=i)
    expect_identical(out, ref)

    # Adding row names.
    rownames(X) <- paste0("CELL", seq_len(nobs))
    preN <- precluster(X)
    i <- sample(rownames(X), 30)
    out <- findKNN(k=k, precomputed=preN, raw.index=TRUE, subset=i)
    ref <- findKNN(t(preN$data), k=k, subset=i)
    expect_identical(out, ref)
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

    # What happens when we request raw.index without precomputed.
    expect_error(findKNN(X, k=20, raw.index=TRUE), "not valid")
})
