# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-findNeighbors.R")

set.seed(999999)

test_that("findNeighbors works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)
    d <- median(findKNN(Y, k=8, get.index=FALSE)$distance[,8])

    out <- findNeighbors(Y, threshold=d)
    ref <- refFindNeighbors(Y, threshold=d)
    expect_equal(out, ref)

#    d <- median(findKNN(Y, k=8, get.index=FALSE, BNPARAM=KmknnParam(distance="Manhattan"))$distance[,8])
#    out <- findNeighbors(Y, threshold=d, BNPARAM=KmknnParam(distance="Manhattan"))
#    ref <- refFindNeighbors(Y, threshold=d, type="manhattan")
#    expect_equal(out, ref)

    d <- median(findKNN(Y, k=8, get.index=FALSE, BNPARAM=KmknnParam(distance="Cosine"))$distance[,8])
    out <- findNeighbors(Y, threshold=d, BNPARAM=KmknnParam(distance="Cosine"))
    Y1 <- Y/sqrt(rowSums(Y^2))
    ref <- findNeighbors(Y1, threshold=d) 
    expect_equal(out, ref)
})

test_that("findNeighbors works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)
    d <- median(findKNN(Y, k=5, get.index=FALSE)$distance[,5])

    out <- findNeighbors(Y, threshold=d)
    pout <- findNeighbors(Y, threshold=d, num.threads=2)
    expect_identical(out, pout)

    pout <- findNeighbors(Y, threshold=d, BPPARAM=BiocParallel::SnowParam(2))
    expect_identical(out, pout)
})

test_that("findNeighbors works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)
    d <- median(findKNN(Y, k=3, get.index=FALSE)$distance[,3])

    out <- findNeighbors(Y, threshold=d)
    sout <- findNeighbors(Y, subset=1:10, threshold=d)
    out$index <- out$index[1:10]
    out$distance <- out$distance[1:10]
    expect_identical(out, sout)

    out <- findNeighbors(Y[0,,drop=FALSE], threshold=d)
    expect_identical(length(out$index), 0L)
    expect_identical(length(out$distance), 0L)
})

test_that("findNeighbors works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)
    d <- median(findKNN(Y, k=3, get.index=FALSE)$distance[,3])

    built <- buildIndex(Y, threshold=d)
    out <- findNeighbors(Y, threshold=d)
    preout <- findNeighbors(built, threshold=d)
    expect_identical(out, preout)

    # Unaffected by BNPARAM settings at this point.
    preout <- findNeighbors(built, threshold=d, BNPARAM=AnnoyParam())
    expect_identical(out, preout)
})

test_that("findNeighbors works with variable outputs", {
    Y <- matrix(rnorm(10000), ncol=20)
    d <- median(findKNN(Y, k=8, get.index=FALSE)$distance[,8])

    out <- findNeighbors(Y, threshold=d)

    iout <- findNeighbors(Y, threshold=d, get.distance=FALSE)
    expect_null(iout$distance)
    expect_identical(iout$index, out$index)

    dout <- findNeighbors(Y, threshold=d, get.index=FALSE)
    expect_null(dout$index)
    expect_identical(dout$distance, out$distance)

    count <- findNeighbors(Y, threshold=d, get.index=FALSE, get.distance=FALSE)
    expect_identical(count, lengths(out$index))
})
