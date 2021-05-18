# Tests queryKmknn().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-query-kmknn.R")

set.seed(1001)
test_that("queryKmknn() behaves correctly with queries", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            out <- queryKmknn(X, k=k, query=Y)
            ref <- refQueryKNN(X, Y, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance)
        }
    }
})

set.seed(1002)
test_that("queryKmknn() works correctly with subsetting", {
    nobs <- 1000
	nquery <- 93
    ndim <- 21
    k <- 7

    X <- matrix(runif(nobs * ndim), nrow=nobs)
	Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- queryKmknn(X, Y, k=k)

    i <- sample(nquery, 20)
    sub <- queryKmknn(X, Y, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nquery, 1, 0.5) == 0L
    sub <- queryKmknn(X, Y, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 50)
    sub <- queryKmknn(X, Y, k=k, subset=i)
    m <- match(i, rownames(Y))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(1003)
test_that("queryKmknn() behaves correctly with alternative options", {
    nobs <- 1000
    nquery <- 100
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    out <- queryKmknn(X, Y, k=k)
    
    # Checking what we extract.
    out2 <- queryKmknn(X, Y, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- queryKmknn(X, Y, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation.
    pre <- buildKmknn(X)
    out4 <- queryKmknn(query=Y, k=k, precomputed=pre) # no need for X!
    expect_identical(out4, out)

    # Checking transposition.
    out5 <- queryKmknn(X, k=k, query=t(Y), transposed=TRUE)
    expect_identical(out5, out)
})

set.seed(1003001)
test_that("queryKmknn() works with Manhattan distances", {
    ndata <- 500 # fewer points as refQueryKNN is a slow brute-force method.
    nquery <- 100

    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            out <- queryKmknn(X, k=k, query=Y, distance="Manhattan")
            ref <- refQueryKNN(X, Y, k=k, type="manhattan")
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance)
        }
    }
})

set.seed(1003002)
test_that("queryKmknn() works correctly with Cosine distances", {
    ndata <- 1000
    nquery <- 100
    ndim <- 5
    k <- 3

    X <- matrix(runif(ndata * ndim), nrow=ndata)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)

    ref <- queryKmknn(X, k=k, query=Y, distance="Cosine")
    out <- queryKmknn(X/sqrt(rowSums(X^2)), k=k, query=Y/sqrt(rowSums(Y^2)))
    expect_identical(ref, out)
})

set.seed(1003001)
test_that("queryKmknn() works to only obtain the last distance", {
    ndata <- 500 # fewer points as refQueryKNN is a slow brute-force method.
    nquery <- 100

    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)

            ref <- queryKmknn(X, k=k, query=Y)
            out <- queryKmknn(X, k=k, query=Y, last=1)
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])

            ref <- queryKmknn(X, k=k, query=Y, distance="Manhattan")
            out <- queryKmknn(X, k=k, query=Y, last=1, distance="Manhattan")
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])
        }
    }
})

set.seed(100301)
test_that("queryKmknn() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1000
    nquery <- 124
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    out <- queryKmknn(X, Y, k=k)
  
    # Trying out different types of parallelization.
    out1 <- queryKmknn(X, Y, k=k, BPPARAM=safeBPParam(2))
    expect_identical(out$index, out1$index)
    expect_identical(out$distance, out1$distance)

    out2 <- queryKmknn(X, Y, k=k, BPPARAM=SnowParam(3))
    expect_identical(out$index, out2$index)
    expect_identical(out$distance, out2$distance)
})

set.seed(10031)
test_that("queryKmknn() raw output behaves correctly", {
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    k <- 7
    X <- matrix(runif(nobs * ndim), nrow=nobs)
  	Y <- matrix(runif(nquery * ndim), nrow=nquery)
 
    pre <- buildKmknn(X)
    out <- queryKmknn(query=Y, k=k, precomputed=pre, raw.index=TRUE)
    ref <- queryKmknn(query=Y, X=t(bndata(pre)), k=k)
    expect_identical(out, ref)

    # Behaves with subsetting.
    i <- sample(nquery, 20)
    out <- queryKmknn(query=Y, k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- queryKmknn(query=Y, X=t(bndata(pre)), k=k, subset=i)
    expect_identical(out, ref)

    i <- rbinom(nquery, 1, 0.5) == 0L
    out <- queryKmknn(query=Y, k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- queryKmknn(query=Y, X=t(bndata(pre)), k=k, subset=i)
    expect_identical(out, ref)

    # Adding row names.
    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 30)
    out <- queryKmknn(query=Y, k=k, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- queryKmknn(query=Y, X=t(bndata(pre)), k=k, subset=i)
    expect_identical(out, ref)
})

set.seed(1004)
test_that("queryKmknn() behaves correctly with silly inputs", {
    nobs <- 1000
	nquery <- 100
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    
    # What happens when k is not positive.
    expect_error(queryKmknn(X, Y, k=0), "positive")
    expect_error(queryKmknn(X, Y, k=-1), "positive")

    # What happens when there are more NNs than k.
    restrict <- 10
    expect_warning(out <- queryKmknn(X[seq_len(restrict),], Y, k=20), "capped")
    expect_warning(ref <- queryKmknn(X[seq_len(restrict),], Y, k=restrict), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- queryKmknn(X[,0], Y[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nquery))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens when the query is of a different dimension.
    Z <- matrix(runif(nobs * ndim * 2), nrow=nobs)
    expect_error(queryKmknn(X, k=20, query=Z), "dimensionality")

    # What happens when we request raw.index without precomputed.
    expect_error(queryKmknn(X, Y, k=20, raw.index=TRUE), "not valid")

    # What happens when the query is not, strictly a matrix.
    AA <- data.frame(Y)
    colnames(AA) <- NULL
    expect_equal(queryKmknn(X, Y, k=20), queryKmknn(X, AA, k=20))

    # What happens with nothing.
    expect_identical(queryKmknn(X, Y, k=10, get.distance=FALSE, get.index=FALSE), list())
})
