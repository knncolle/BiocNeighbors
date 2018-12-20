# Tests for correct dispatch with S4 methods.
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-methods.R")

set.seed(100)
nobs <- 1000
ndim <- 10
k <- 5
X <- matrix(runif(nobs * ndim), nrow=nobs)

test_that("buildIndex dispatches correctly", {
    set.seed(100)        
    out <- buildIndex(X)
    set.seed(100)        
    out2 <- buildIndex(X, BNPARAM=KmknnParam())
    expect_equal(out, out2)
    expect_s4_class(out, "KmknnIndex")
    
    # Passes arguments down.
    set.seed(100)
    out <- buildIndex(X, BNPARAM=KmknnParam(iter.max=20))
    set.seed(100)
    ref <- buildKmknn(X, iter.max=20)
    expect_equal(out, ref)

    # Handles Vptree.
    out <- buildIndex(X, BNPARAM=VptreeParam())
    expect_s4_class(out, "VptreeIndex")

    # Handles Annoy:
    out <- buildIndex(X, BNPARAM=AnnoyParam())
    expect_s4_class(out, "AnnoyIndex")

    # Handles Hnsw:
    out <- buildIndex(X, BNPARAM=HnswParam())
    expect_s4_class(out, "HnswIndex")
})

#######################################

test_that("findKNN dispatches correctly for KMKNN", {
    # Equivalent calls to Kmknn:
    out1 <- findKNN(X, k=10) 
    out2 <- findKNN(X, k=10, BNINDEX=buildKmknn(X)) 
    out3 <- findKNN(X, k=10, BNPARAM=KmknnParam()) 
    out4 <- findKNN(X, k=10, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_equal(out1, out2)
    expect_equal(out1, out3)
    expect_equal(out1, out4)

    # Testing the behaviour of the NULL methods.
    out5 <- findKNN(X, k=10, BNINDEX=NULL)
    out6 <- findKNN(X, k=10, BNPARAM=NULL)
    out7 <- findKNN(X, k=10, BNINDEX=NULL, BNPARAM=KmknnParam())
    out8 <- findKNN(X, k=10, BNINDEX=buildKmknn(X), BNPARAM=NULL)
    expect_equal(out1, out5)
    expect_equal(out1, out6)
    expect_equal(out1, out7)
    expect_equal(out1, out8)
})

test_that("findKNN dispatches correctly for VP trees", {
    # Equivalent calls to Kmknn:
    out1 <- findKNN(X, k=10, BNINDEX=buildVptree(X)) 
    out2 <- findKNN(X, k=10, BNPARAM=VptreeParam()) 
    out3 <- findKNN(X, k=10, BNPARAM=VptreeParam(), BNINDEX=buildVptree(X)) 
    expect_equal(out1, out2)
    expect_equal(out1, out3)

    # Testing the behaviour of the NULL methods.
    out4 <- findKNN(X, k=10, BNINDEX=NULL, BNPARAM=VptreeParam())
    out5 <- findKNN(X, k=10, BNINDEX=buildVptree(X), BNPARAM=NULL)
    expect_equal(out1, out4)
    expect_equal(out1, out5)
})

test_that("findKNN dispatches correctly for Annoy", {
    # Equivalent calls to Annoy: 
    outA <- findKNN(X, k=10, BNINDEX=buildAnnoy(X)) 
    outB <- findKNN(X, k=10, BNPARAM=AnnoyParam()) 
    outC <- findKNN(X, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X)) 
    expect_equal(outA, outB)
    expect_equal(outA, outC)
    expect_false(isTRUE(all.equal(outA, findKNN(X, k=10)))) # Checking that they're not exact.

    # Testing the behaviour of the NULL methods.
    outD <- findKNN(X, k=10, BNINDEX=buildAnnoy(X), BNPARAM=NULL)
    outE <- findKNN(X, k=10, BNINDEX=NULL, BNPARAM=AnnoyParam())
    expect_equal(outA, outD)
    expect_equal(outA, outE)

    # Parameters get passed down.
    alt1 <- findKNN(X, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X, ntrees=200)) 
    alt2 <- findKNN(X, k=10, BNPARAM=AnnoyParam(ntrees=200)) 
    expect_equal(alt1, alt2)
    expect_false(isTRUE(all.equal(outA, alt1))) # Checking that they're not exact.
})

test_that("findKNN dispatches correctly for HNSW", {
    # Equivalent calls to Hnsw: 
    outA <- findKNN(X, k=10, BNINDEX=buildHnsw(X)) 
    outB <- findKNN(X, k=10, BNPARAM=HnswParam()) 
    outC <- findKNN(X, k=10, BNPARAM=HnswParam(), BNINDEX=buildHnsw(X)) 
    expect_equal(outA, outB)
    expect_equal(outA, outC)
    expect_false(isTRUE(all.equal(outA, findKNN(X, k=10)))) # Checking that they're not exact.

    # Testing the behaviour of the NULL methods.
    outD <- findKNN(X, k=10, BNINDEX=buildHnsw(X), BNPARAM=NULL)
    outE <- findKNN(X, k=10, BNINDEX=NULL, BNPARAM=HnswParam())
    expect_equal(outA, outD)
    expect_equal(outA, outE)

    # Parameters get passed down.
    alt1 <- findKNN(X, k=10, BNPARAM=HnswParam(), BNINDEX=buildHnsw(X, nlinks=20)) 
    alt2 <- findKNN(X, k=10, BNPARAM=HnswParam(nlinks=20)) 
    expect_equal(alt1, alt2)
    expect_false(isTRUE(all.equal(outA, alt1))) # Checking that they're not exact.
})

test_that("Illegal findKNN signatures fail", {
    expect_error(findKNN(X, BNPARAM=AnnoyParam(), BNINDEX=buildKmknn(X)), "unable to find an inherited method")
    expect_error(findKNN(X, BNPARAM=KmknnParam(), BNINDEX=buildAnnoy(X)), "unable to find an inherited method")
})

#######################################

set.seed(101)
nquery <- 500
Y <- matrix(runif(nquery * ndim), nrow=nquery)

test_that("queryKNN dispatches correctly for KMKNN", {
    # Equivalent calls to Kmknn:
    out1 <- queryKNN(X, Y, k=10) 
    out2 <- queryKNN(X, Y, k=10, BNINDEX=buildKmknn(X)) 
    out3 <- queryKNN(X, Y, k=10, BNPARAM=KmknnParam()) 
    out4 <- queryKNN(X, Y, k=10, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_equal(out1, out2)
    expect_equal(out1, out3)
    expect_equal(out1, out4)

    # Testing the behaviour of the NULL methods.
    out5 <- queryKNN(X, Y, k=10, BNINDEX=NULL)
    out6 <- queryKNN(X, Y, k=10, BNPARAM=NULL)
    out7 <- queryKNN(X, Y, k=10, BNINDEX=NULL, BNPARAM=KmknnParam())
    out8 <- queryKNN(X, Y, k=10, BNINDEX=buildKmknn(X), BNPARAM=NULL)
    expect_equal(out1, out5)
    expect_equal(out1, out6)
    expect_equal(out1, out7)
    expect_equal(out1, out8)
})

test_that("queryKNN dispatches correctly for VP trees", {
    # Equivalent calls to Kmknn:
    out1 <- queryKNN(X, Y, k=10, BNINDEX=buildKmknn(X)) 
    out2 <- queryKNN(X, Y, k=10, BNPARAM=KmknnParam()) 
    out3 <- queryKNN(X, Y, k=10, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_equal(out1, out2)
    expect_equal(out1, out3)

    # Testing the behaviour of the NULL methods.
    out4 <- queryKNN(X, Y, k=10, BNINDEX=NULL, BNPARAM=KmknnParam())
    out5 <- queryKNN(X, Y, k=10, BNINDEX=buildKmknn(X), BNPARAM=NULL)
    expect_equal(out1, out4)
    expect_equal(out1, out5)
})

test_that("queryKNN dispatches correctly for Annoy", {
    # Equivalent calls to Annoy: 
    outA <- queryKNN(X, Y, k=10, BNINDEX=buildAnnoy(X)) 
    outB <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam()) 
    outC <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X)) 
    expect_equal(outA, outB)
    expect_equal(outA, outC)
    expect_false(isTRUE(all.equal(outA, queryKNN(X, Y, k=10)))) # Checking that they're not exact.

    # Testing the behaviour of the NULL methods.
    outD <- queryKNN(X, Y, k=10, BNINDEX=buildAnnoy(X), BNPARAM=NULL)
    outE <- queryKNN(X, Y, k=10, BNINDEX=NULL, BNPARAM=AnnoyParam())
    expect_equal(outA, outD)
    expect_equal(outA, outE)

    # Parameters get passed down.
    alt1 <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam(), BNINDEX=buildAnnoy(X, ntrees=200)) 
    alt2 <- queryKNN(X, Y, k=10, BNPARAM=AnnoyParam(ntrees=200)) 
    expect_equal(alt1, alt2)
    expect_false(isTRUE(all.equal(outA, alt1))) # Checking that they're not exact.
})

test_that("queryKNN dispatches correctly for Hnsw", {
    # Equivalent calls to Hnsw: 
    outA <- queryKNN(X, Y, k=10, BNINDEX=buildHnsw(X)) 
    outB <- queryKNN(X, Y, k=10, BNPARAM=HnswParam()) 
    outC <- queryKNN(X, Y, k=10, BNPARAM=HnswParam(), BNINDEX=buildHnsw(X)) 
    expect_equal(outA, outB)
    expect_equal(outA, outC)
    expect_false(isTRUE(all.equal(outA, queryKNN(X, Y, k=10)))) # Checking that they're not exact.

    # Testing the behaviour of the NULL methods.
    outD <- queryKNN(X, Y, k=10, BNINDEX=buildHnsw(X), BNPARAM=NULL)
    outE <- queryKNN(X, Y, k=10, BNINDEX=NULL, BNPARAM=HnswParam())
    expect_equal(outA, outD)
    expect_equal(outA, outE)

    # Parameters get passed down.
    alt1 <- queryKNN(X, Y, k=10, BNPARAM=HnswParam(), BNINDEX=buildHnsw(X, nlinks=25)) 
    alt2 <- queryKNN(X, Y, k=10, BNPARAM=HnswParam(nlinks=25)) 
    expect_equal(alt1, alt2)
    expect_false(isTRUE(all.equal(outA, alt1))) # Checking that they're not exact.
})

test_that("Illegal queryKNN signatures fail", {
    expect_error(queryKNN(X, BNPARAM=AnnoyParam(), BNINDEX=buildKmknn(X)), "unable to find an inherited method")
    expect_error(queryKNN(X, BNPARAM=KmknnParam(), BNINDEX=buildAnnoy(X)), "unable to find an inherited method")
})

#######################################

test_that("findNeighbors dispatches correctly for KMKNN", {
    # Equivalent calls to Kmknn:
    out1 <- findNeighbors(X, threshold=1) 
    out2 <- findNeighbors(X, threshold=1, BNINDEX=buildKmknn(X)) 
    out3 <- findNeighbors(X, threshold=1, BNPARAM=KmknnParam()) 
    out4 <- findNeighbors(X, threshold=1, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_identical_re(out1, out2)
    expect_identical_re(out1, out3)
    expect_identical_re(out1, out4)

    # Testing the behaviour of the NULL methods.
    out5 <- findNeighbors(X, threshold=1, BNINDEX=NULL)
    out6 <- findNeighbors(X, threshold=1, BNPARAM=NULL)
    out7 <- findNeighbors(X, threshold=1, BNINDEX=NULL, BNPARAM=KmknnParam())
    out8 <- findNeighbors(X, threshold=1, BNINDEX=buildKmknn(X), BNPARAM=NULL)
    expect_identical_re(out1, out5)
    expect_identical_re(out1, out6)
    expect_identical_re(out1, out7)
    expect_identical_re(out1, out8)
})

test_that("findNeighbors dispatches correctly for VP trees", {
    # Equivalent calls to Kmknn:
    out1 <- findNeighbors(X, threshold=1, BNINDEX=buildVptree(X)) 
    out2 <- findNeighbors(X, threshold=1, BNPARAM=VptreeParam()) 
    out3 <- findNeighbors(X, threshold=1, BNPARAM=VptreeParam(), BNINDEX=buildVptree(X)) 
    expect_identical_re(out1, out2)
    expect_identical_re(out1, out3)

    # Testing the behaviour of the NULL methods.
    out4 <- findNeighbors(X, threshold=1, BNINDEX=NULL, BNPARAM=VptreeParam())
    out5 <- findNeighbors(X, threshold=1, BNINDEX=buildVptree(X), BNPARAM=NULL)
    expect_identical_re(out1, out4)
    expect_identical_re(out1, out5)
})

test_that("Illegal findNeighbors signatures fail", {
    expect_error(findNeighbors(X, BNPARAM=VptreeParam(), BNINDEX=buildKmknn(X)), "unable to find an inherited method")
    expect_error(findNeighbors(X, BNPARAM=KmknnParam(), BNINDEX=buildVptree(X)), "unable to find an inherited method")
})

#######################################

set.seed(101)
nquery <- 500
Y <- matrix(runif(nquery * ndim), nrow=nquery)

test_that("queryNeighbors dispatches correctly for KMNeighbors", {
    # Equivalent calls to Kmknn:
    out1 <- queryNeighbors(X, Y, threshold=1) 
    out2 <- queryNeighbors(X, Y, threshold=1, BNINDEX=buildKmknn(X)) 
    out3 <- queryNeighbors(X, Y, threshold=1, BNPARAM=KmknnParam()) 
    out4 <- queryNeighbors(X, Y, threshold=1, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_identical_re(out1, out2)
    expect_identical_re(out1, out3)
    expect_identical_re(out1, out4)

    # Testing the behaviour of the NULL methods.
    out5 <- queryNeighbors(X, Y, threshold=1, BNINDEX=NULL)
    out6 <- queryNeighbors(X, Y, threshold=1, BNPARAM=NULL)
    out7 <- queryNeighbors(X, Y, threshold=1, BNINDEX=NULL, BNPARAM=KmknnParam())
    out8 <- queryNeighbors(X, Y, threshold=1, BNINDEX=buildKmknn(X), BNPARAM=NULL)
    expect_identical_re(out1, out5)
    expect_identical_re(out1, out6)
    expect_identical_re(out1, out7)
    expect_identical_re(out1, out8)
})

test_that("queryNeighbors dispatches correctly for VP trees", {
    # Equivalent calls to Kmknn:
    out1 <- queryNeighbors(X, Y, threshold=1, BNINDEX=buildKmknn(X)) 
    out2 <- queryNeighbors(X, Y, threshold=1, BNPARAM=KmknnParam()) 
    out3 <- queryNeighbors(X, Y, threshold=1, BNPARAM=KmknnParam(), BNINDEX=buildKmknn(X)) 
    expect_identical_re(out1, out2)
    expect_identical_re(out1, out3)

    # Testing the behaviour of the NULL methods.
    out4 <- queryNeighbors(X, Y, threshold=1, BNINDEX=NULL, BNPARAM=KmknnParam())
    out5 <- queryNeighbors(X, Y, threshold=1, BNINDEX=buildKmknn(X), BNPARAM=NULL)
    expect_identical_re(out1, out4)
    expect_identical_re(out1, out5)
})

test_that("Illegal queryNeighbors signatures fail", {
    expect_error(queryNeighbors(X, BNPARAM=VptreeParam(), BNINDEX=buildKmknn(X)), "unable to find an inherited method")
    expect_error(queryNeighbors(X, BNPARAM=KmknnParam(), BNINDEX=buildVptree(X)), "unable to find an inherited method")
})


