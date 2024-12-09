# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-queryNeighbors.R")

set.seed(999999)

test_that("queryNeighbors works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryDistance(Y, Z, k=8))

    out <- queryNeighbors(Y, Z, threshold=d)
    ref <- refQueryNeighbors(Y, Z, threshold=d)
    expect_equal(out, ref)

    d <- median(queryDistance(Y, Z, k=8, BNPARAM=KmknnParam(distance="Manhattan")))
    out <- queryNeighbors(Y, Z, threshold=d, BNPARAM=KmknnParam(distance="Manhattan"))
    ref <- refQueryNeighbors(Y, Z, threshold=d, type="manhattan")
    expect_equal(out, ref)

    expect_error(queryNeighbors(Y, Z[,0], threshold=d), "mismatch")
})

test_that("queryNeighbors works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryDistance(Y, Z, k=5))

    out <- queryNeighbors(Y, Z, threshold=d)
    pout <- queryNeighbors(Y, Z, threshold=d, num.threads=2)
    expect_identical(out, pout)

    pout <- queryNeighbors(Y, Z, threshold=d, BPPARAM=BiocParallel::SnowParam(2))
    expect_identical(out, pout)
})

test_that("queryNeighbors works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryDistance(Y, Z, k=3))

    out <- queryNeighbors(Y, Z, threshold=d)
    sout <- queryNeighbors(Y, Z, subset=1:10, threshold=d)
    out$index <- out$index[1:10]
    out$distance <- out$distance[1:10]
    expect_identical(out, sout)

    expect_error(queryNeighbors(Y, Z, threshold=d, subset=100000), "out of bounds")
    out <- queryNeighbors(Y[0,,drop=FALSE], Z, threshold=d)
    expect_identical(lengths(out$index), integer(nrow(Z)))
    expect_identical(lengths(out$distance), integer(nrow(Z)))
})

test_that("queryNeighbors works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryDistance(Y, Z, k=3))

    built <- buildIndex(Y, threshold=d)
    out <- queryNeighbors(Y, Z, threshold=d)
    preout <- queryNeighbors(built, Z, threshold=d)
    expect_identical(out, preout)

    # Unaffected by BNPARAM settings at this point.
    preout <- queryNeighbors(built, Z, threshold=d, BNPARAM=AnnoyParam())
    expect_identical(out, preout)

    # Throws an error for deserialized prebuilt indices.
    tmp <- tempfile(fileext=".rds")
    saveRDS(built, tmp)
    expect_error(queryNeighbors(readRDS(tmp), Z, threshold=d), "null pointer")
})

test_that("queryNeighbors works when inputs are transposed", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryDistance(Y, Z, k=3))

    out <- queryNeighbors(Y, Z, threshold=d)
    tout <- queryNeighbors(t(Y), t(Z), threshold=d, transposed=TRUE)
    expect_identical(out, tout)
})

test_that("queryNeighbors works with variable thresholds", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d4 <- median(queryDistance(Y, Z, k=4))
    d10 <- median(queryDistance(Y, Z, k=10))

    th <- rep(c(d4, d10), length.out=nrow(Z))
    out <- queryNeighbors(Y, Z, threshold=th)

    keep <- th == d4
    ref <- queryNeighbors(Y, Z, threshold=d4)
    expect_identical(out$index[keep], ref$index[keep])
    expect_identical(out$distance[keep], ref$distance[keep])

    keep <- th == d10
    ref <- queryNeighbors(Y, Z, threshold=d10)
    expect_identical(out$index[keep], ref$index[keep])
    expect_identical(out$distance[keep], ref$distance[keep])

    expect_error(queryNeighbors(Y, Z, threshold=1:10), "should have length equal")
})

test_that("queryNeighbors works with variable outputs", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryDistance(Y, Z, k=8))

    out <- queryNeighbors(Y, Z, threshold=d)

    iout <- queryNeighbors(Y, Z, threshold=d, get.distance=FALSE)
    expect_null(iout$distance)
    expect_identical(iout$index, out$index)

    dout <- queryNeighbors(Y, Z, threshold=d, get.index=FALSE)
    expect_null(dout$index)
    expect_identical(dout$distance, out$distance)

    count <- queryNeighbors(Y, Z, threshold=d, get.index=FALSE, get.distance=FALSE)
    expect_identical(count, lengths(out$index))
})
