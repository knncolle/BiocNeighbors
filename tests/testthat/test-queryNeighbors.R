# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-queryNeighbors.R")

set.seed(999999)

test_that("queryNeighbors works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryKNN(Y, Z, k=8, get.index=FALSE)$distance[,8])

    out <- queryNeighbors(Y, Z, threshold=d)
    ref <- refQueryNeighbors(Y, Z, threshold=d)
    expect_equal(out, ref)

#    d <- median(queryKNN(Y, Z, k=8, get.index=FALSE, BNPARAM=KmknnParam(distance="Manhattan"))$distance[,8])
#    out <- queryNeighbors(Y, Z, threshold=d, BNPARAM=KmknnParam(distance="Manhattan"))
#    ref <- refQueryNeighbors(Y, Z, threshold=d, type="manhattan")
#    expect_equal(out, ref)
})

test_that("queryNeighbors works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryKNN(Y, Z, k=5, get.index=FALSE)$distance[,5])

    out <- queryNeighbors(Y, Z, threshold=d)
    pout <- queryNeighbors(Y, Z, threshold=d, num.threads=2)
    expect_identical(out, pout)

    pout <- queryNeighbors(Y, Z, threshold=d, BPPARAM=BiocParallel::SnowParam(2))
    expect_identical(out, pout)
})

test_that("queryNeighbors works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryKNN(Y, Z, k=3, get.index=FALSE)$distance[,3])

    out <- queryNeighbors(Y, Z, threshold=d)
    sout <- queryNeighbors(Y, Z, subset=1:10, threshold=d)
    out$index <- out$index[1:10]
    out$distance <- out$distance[1:10]
    expect_identical(out, sout)

    out <- queryNeighbors(Y[0,,drop=FALSE], Z, threshold=d)
    expect_identical(lengths(out$index), integer(nrow(Z)))
    expect_identical(lengths(out$distance), integer(nrow(Z)))
})

test_that("queryNeighbors works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryKNN(Y, Z, k=3, get.index=FALSE)$distance[,3])

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
    d <- median(queryKNN(Y, Z, k=3, get.index=FALSE)$distance[,3])

    out <- queryNeighbors(Y, Z, threshold=d)
    tout <- queryNeighbors(t(Y), t(Z), threshold=d, transposed=TRUE)
    expect_identical(out, tout)
})

test_that("queryNeighbors works with variable outputs", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)
    d <- median(queryKNN(Y, Z, k=8, get.index=FALSE)$distance[,8])

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
