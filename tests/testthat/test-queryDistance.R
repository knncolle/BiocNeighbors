# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-queryDistance.R")

set.seed(77777)

test_that("queryDistance works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryDistance(Y, Z, k=8)
    ref <- queryKNN(Y, Z, k=8)
    expect_equal(ref$distance[,8], out)

    # Respects alternative methods.
    adist <- queryDistance(Y, Z, k=8, BNPARAM=AnnoyParam())
    expect_false(identical(dist, adist))
})

test_that("queryDistance works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryDistance(Y, Z, k=8)
    pout <- queryDistance(Y, Z, k=8, num.threads=2)
    expect_equal(out, pout)
})

test_that("queryDistance works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryDistance(Y, Z, k=8)
    sout <- queryDistance(Y, Z, subset=1:10, k=8)
    expect_equal(out[1:10], sout)

    expect_warning(out <- queryDistance(Y[0,,drop=FALSE], Z, k=8), "capped")
    expect_identical(out, numeric(nrow(Z)))
})

test_that("queryDistance works with variable k", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    k <- rep(c(4, 10), length.out=nrow(Z))
    out <- queryDistance(Y, Z, k=k)

    keep <- k == 4
    ref <- queryDistance(Y, Z, k=4)
    expect_identical(out[keep], ref[keep])

    keep <- k == 10
    ref <- queryDistance(Y, Z, k=10)
    expect_identical(out[keep], ref[keep])

    # AsIs has no effect here.
    out <- queryDistance(Y, Z, k=10, subset=1)
    ref <- queryDistance(Y, Z, k=I(10), subset=1)
    expect_identical(out, ref)
})

test_that("queryDistance works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    built <- buildIndex(Y, k=8)
    out <- queryDistance(Y, Z, k=8)
    preout <- queryDistance(built, Z, k=8)
    expect_identical(out, preout)
})

test_that("queryDistance works when inputs are transposed", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryDistance(Y, Z, k=8)
    tout <- queryDistance(t(Y), t(Z), k=8, transposed=TRUE)
    expect_identical(out, tout)
})
