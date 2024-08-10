# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-findDistance.R")

set.seed(888888)

test_that("findDistance works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)

    dist <- findDistance(Y, k=8)
    ref <- findKNN(Y, k=8)
    expect_identical(ref$distance[,8], dist)

    # Respects alternative methods.
    adist <- findDistance(Y, k=8, BNPARAM=AnnoyParam())
    expect_false(identical(dist, adist))
})

test_that("findDistance works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findDistance(Y, k=8)
    dist <- findDistance(Y, k=8, num.threads=2)
    expect_identical(out[,8], dist)
})

test_that("findDistance works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findDistance(Y, k=8)
    dist <- findDistance(Y, k=8, subset=1:10)
    expect_identical(out[1:10], dist)

    expect_warning(dist <- findDistance(Y[0,,drop=FALSE], k=8), "capped")
    expect_identical(length(dist), 0L)
})

test_that("findDistance works with variable k", {
    Y <- matrix(rnorm(10000), ncol=20)

    k <- rep(c(4, 10), length.out=nrow(Y))
    out <- findDistance(Y, k=k)

    keep <- k == 4
    ref <- findDistance(Y, k=4)
    expect_identical(ref[keep], out[keep])

    keep <- k == 10
    ref <- findDistance(Y, k=10)
    expect_identical(ref[keep], out[keep])

    # AsIs forced variable makes no difference here.
    out <- findDistance(Y, k=I(10), subset=1)
    ref <- findDistance(Y, k=10, subset=1)
    expect_identical(out, ref)
})

test_that("findDistance works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)

    built <- buildIndex(Y, k=8)
    dist <- findDistance(Y, k=8)
    predist <- findDistance(built, k=8)
    expect_identical(dist, predist)
})
