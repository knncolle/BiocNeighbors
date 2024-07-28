# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-queryKNN.R")

set.seed(999999)

test_that("queryKNN works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryKNN(Y, Z, k=8)
    ref <- refQueryKNN(Y, Z, k=8)
    expect_equal(out, ref)

    out <- queryKNN(Y, Z, k=8, BNPARAM=KmknnParam(distance="Manhattan"))
    ref <- refQueryKNN(Y, Z, k=8, type="manhattan")
    expect_equal(out, ref)
})

test_that("queryKNN works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryKNN(Y, Z, k=8)
    pout <- queryKNN(Y, Z, k=8, num.threads=2)
    expect_equal(out, pout)

    pout <- queryKNN(Y, Z, k=8, BPPARAM=BiocParallel::SnowParam(2))
    expect_equal(out, pout)
})

test_that("queryKNN works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryKNN(Y, Z, k=8)
    sout <- queryKNN(Y, Z, subset=1:10, k=8)
    out$index <- out$index[1:10,]
    out$distance <- out$distance[1:10,]
    expect_equal(out, sout)

    expect_warning(out <- queryKNN(Y[0,,drop=FALSE], Z, k=8), "capped")
    expect_identical(ncol(out$index), 0L)
    expect_identical(ncol(out$distance), 0L)
})

test_that("queryKNN works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    built <- buildIndex(Y, k=8)
    out <- queryKNN(Y, Z, k=8)
    preout <- queryKNN(built, Z, k=8)
    expect_identical(out, preout)

    # Unaffected by BNPARAM settings at this point.
    preout <- queryKNN(built, Z, k=8, BNPARAM=AnnoyParam())
    expect_identical(out, preout)
})

test_that("queryKNN works when inputs are transposed", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryKNN(Y, Z, k=8)
    tout <- queryKNN(t(Y), t(Z), k=8, transposed=TRUE)
    expect_identical(out, tout)
})

test_that("queryKNN works with variable outputs", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    out <- queryKNN(Y, Z, k=8)

    iout <- queryKNN(Y, Z, k=8, get.distance=FALSE)
    expect_null(iout$distance)
    expect_identical(iout$index, out$index)

    dout <- queryKNN(Y, Z, k=8, get.index=FALSE)
    expect_null(dout$index)
    expect_identical(dout$distance, out$distance)

    tout <- queryKNN(Y, Z, k=8, get.index="transposed", get.distance="transposed")
    expect_identical(t(tout$distance), out$distance)
    expect_identical(t(tout$index), out$index)
})
