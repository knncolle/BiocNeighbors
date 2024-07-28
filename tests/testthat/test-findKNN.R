# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-findKNN.R")

set.seed(999999)

test_that("findKNN works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)
    ref <- refFindKNN(Y, k=8)
    expect_equal(out, ref)

    out <- findKNN(Y, k=8, BNPARAM=KmknnParam(distance="Manhattan"))
    ref <- refFindKNN(Y, k=8, type="manhattan")
    expect_equal(out, ref)
})

test_that("findKNN works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)
    pout <- findKNN(Y, k=8, num.threads=2)
    expect_identical(out, pout)

    pout <- findKNN(Y, k=8, BPPARAM=BiocParallel::SnowParam(2))
    expect_identical(out, pout)
})

test_that("findKNN works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)
    sout <- findKNN(Y, subset=1:10, k=8)
    out$index <- out$index[1:10,]
    out$distance <- out$distance[1:10,]
    expect_identical(out, sout)

    expect_warning(out <- findKNN(Y[0,,drop=FALSE], k=8), "capped")
    expect_identical(ncol(out$index), 0L)
    expect_identical(ncol(out$distance), 0L)
})

test_that("findKNN works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)

    built <- buildIndex(Y, k=8)
    out <- findKNN(Y, k=8)
    preout <- findKNN(built, k=8)
    expect_identical(out, preout)

    # Unaffected by BNPARAM settings at this point.
    preout <- findKNN(built, k=8, BNPARAM=AnnoyParam())
    expect_identical(out, preout)
})

test_that("findKNN works with variable outputs", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)

    iout <- findKNN(Y, k=8, get.distance=FALSE)
    expect_null(iout$distance)
    expect_identical(iout$index, out$index)

    dout <- findKNN(Y, k=8, get.index=FALSE)
    expect_null(dout$index)
    expect_identical(dout$distance, out$distance)

    tout <- findKNN(Y, k=8, get.index="transposed", get.distance="transposed")
    expect_identical(t(tout$distance), out$distance)
    expect_identical(t(tout$index), out$index)
})
