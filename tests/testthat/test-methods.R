# Tests for correct dispatch with S4 methods.
# library(BiocNeighbors); library(testthat); source("test-methods.R")

set.seed(100)
nobs <- 1000
ndim <- 10
k <- 5
X <- matrix(runif(nobs * ndim), nrow=nobs)

test_that("buildNNIndex dispatches correctly", {
    set.seed(100)        
    out <- buildNNIndex(X)
    set.seed(100)        
    out2 <- buildNNIndex(X, BNPARAM=KmknnParam())
    expect_equal(out, out2)
    expect_s4_class(out, "KmknnIndex")
    
    # Passes arguments down.
    set.seed(100)
    out <- buildNNIndex(X, BNPARAM=KmknnParam(iter.max=20))
    set.seed(100)
    ref <- buildKmknn(X, iter.max=20)
    expect_equal(out, ref)

    # Handles Annoy:
    out <- buildNNIndex(X, BNPARAM=AnnoyParam())
    expect_s4_class(out, "AnnoyIndex")
})

test_that("findKNN dispatches correctly", {
    # Equivalent calls to Kmknn:
    out1 <- findKNN(X, k=10) 
    out2 <- findKNN(X, k=10, BNINDEX=buildKmknn(X)) 
    out3 <- findKNN(X, k=10, BNPARAM=KmknnParam()) 
    out4 <- findKNN(X, k=10, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_equal(out1, out2)
    expect_equal(out1, out3)
    expect_equal(out1, out4)

    # Equivalent calls to Annoy: 
    outA <- findKNN(X, k=10, BNINDEX=buildAnnoy(X)) 
    outB <- findKNN(X, k=10, BNPARAM=AnnoyParam()) 
    outC <- findKNN(X, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X)) 
    expect_equal(outA, outB)
    expect_equal(outA, outC)
    expect_false(isTRUE(all.equal(outA, out1))) # Checking that they're not exact.

    # Parameters get passed down.
    alt1 <- findKNN(X, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X, ntrees=200)) 
    alt2 <- findKNN(X, k=10, BNPARAM=AnnoyParam(ntrees=200)) 
    expect_equal(alt1, alt2)
    expect_false(isTRUE(all.equal(outA, alt1))) # Checking that they're not exact.

    expect_error(findKNN(X, BNPARAM=AnnoyParam(), BNINDEX=buildKmknn(X)), "unable to find an inherited method")
    expect_error(findKNN(X, BNPARAM=KmknnParam(), BNINDEX=buildAnnoy(X)), "unable to find an inherited method")
})

test_that("queryKNN dispatches correctly", {
    nquery <- 500
    Y <- matrix(runif(nquery * ndim), nrow=nquery)

    # Equivalent calls to Kmknn:
    out1 <- queryKNN(X, Y, k=10) 
    out2 <- queryKNN(X, Y, k=10, BNINDEX=buildKmknn(X)) 
    out3 <- queryKNN(X, Y, k=10, BNPARAM=KmknnParam()) 
    out4 <- queryKNN(X, Y, k=10, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_equal(out1, out2)
    expect_equal(out1, out3)
    expect_equal(out1, out4)

    # Equivalent calls to Annoy: 
    outA <- queryKNN(X, Y, k=10, BNINDEX=buildAnnoy(X)) 
    outB <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam()) 
    outC <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X)) 
    expect_equal(outA, outB)
    expect_equal(outA, outC)
    expect_false(isTRUE(all.equal(outA, out1))) # Checking that they're not exact.

    # Parameters get passed down.
    alt1 <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X, ntrees=200)) 
    alt2 <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam(ntrees=200)) 
    expect_equal(alt1, alt2)
    expect_false(isTRUE(all.equal(outA, alt1))) # Checking that they're not exact.

    expect_error(queryKNN(X, BNPARAM=AnnoyParam(), BNINDEX=buildKmknn(X)), "unable to find an inherited method")
    expect_error(queryKNN(X, BNPARAM=KmknnParam(), BNINDEX=buildAnnoy(X)), "unable to find an inherited method")
})
