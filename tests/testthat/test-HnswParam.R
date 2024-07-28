# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-HnswParam.R")

test_that("HnswParam construction behaves properly", {
    p <- HnswParam()
    expect_output(show(p), "HnswParam")
    expect_identical(bndistance(p), "Euclidean")

    p <- HnswParam(distance="Manhattan")
    expect_identical(bndistance(p), "Manhattan")
})

test_that("Cursory checks for HnswParam", {
    Y <- matrix(rnorm(10000), ncol=20)

    p <- HnswParam()
    out <- findKNN(Y, k=8, BNPARAM=p)
    expect_identical(ncol(out$distance), 8L)
    expect_identical(ncol(out$index), 8L)
})

test_that("HnswParam queries behave with cosine distance", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    p <- HnswParam(distance="Cosine")

    out <- queryKNN(Y, Z, k=8, BNPARAM=p)
    Y1 <- Y/sqrt(rowSums(Y^2))
    Z1 <- Z/sqrt(rowSums(Z^2))
    ref <- queryKNN(Y1, Z1, k=8, BNPARAM=p)
    expect_equal(out, ref)
})
