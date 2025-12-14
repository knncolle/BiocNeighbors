# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-VptreeParam.R")

test_that("VptreeParam construction behaves properly", {
    p <- VptreeParam()
    expect_output(show(p), "VptreeParam")
    expect_identical(bndistance(p), "Euclidean")

    p <- VptreeParam(distance="Manhattan")
    expect_identical(bndistance(p), "Manhattan")
})

test_that("Cursory checks for VptreeParam", {
    Y <- matrix(rnorm(10000), ncol=20)

    p <- VptreeParam()
    out <- findKNN(Y, k=8, BNPARAM=p)
    expect_identical(ncol(out$distance), 8L)
    expect_identical(ncol(out$index), 8L)

    # Slightly increase distance to avoid a direct comparison to the median.
    # Otherwise, we'd have one distance that is equal to the median,
    # and that equality might be broken by floating-point imprecision.
    d <- median(out$distance[,ncol(out$distance)]) * 1.000001

    allout <- findNeighbors(Y, threshold=d, BNPARAM=p)
    expect_identical(length(allout$distance), nrow(Y))
    expect_identical(length(allout$index), nrow(Y))
})

test_that("VptreeParam queries behave with cosine distance", {
    Y <- matrix(rnorm(10000), ncol=20)
    Z <- matrix(rnorm(2000), ncol=20)

    p <- VptreeParam(distance="Cosine")

    out <- queryKNN(Y, Z, k=8, BNPARAM=p)
    Y1 <- Y/sqrt(rowSums(Y^2))
    Z1 <- Z/sqrt(rowSums(Z^2))
    ref <- queryKNN(Y1, Z1, k=8, BNPARAM=p)
    expect_equal(out, ref)

    d <- median(out$distance[,ncol(out$distance)]) * 1.000001
    allout <- queryNeighbors(Y, Z, threshold=d, BNPARAM=p)
    Y1 <- Y/sqrt(rowSums(Y^2))
    Z1 <- Z/sqrt(rowSums(Z^2))
    allref <- queryNeighbors(Y1, Z1, threshold=d, BNPARAM=p)
    expect_equal(out, ref)
})
