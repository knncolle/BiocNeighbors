# library(testthat); library(BiocNeighbors); source("test-convert.R")

set.seed(99999)

test_that("coversion to sparseMatrix works", {
    X <- matrix(rnorm(10000), ncol = 20)
    N <- nrow(X)
    out <- findKNN(X, k = 8)

    res <- convertToSparseMatrix(out)
    expect_s4_class(res, "sparseMatrix")
    expect_identical(nrow(res), N)
    expect_identical(ncol(res), N)

    expected.first <- numeric(N)
    expected.first[out$index[1,]] <- out$distance[1,]
    expect_identical(expected.first, res[1,])

    expected.last <- numeric(N)
    expected.last[out$index[N,]] <- out$distance[N,]
    expect_identical(expected.last, res[N,])
})

test_that("coversion to SelfHits works", {
    X <- matrix(rnorm(10000), ncol = 20)
    N <- nrow(X)
    out <- findKNN(X, k = 8)

    res <- convertToSelfHits(out)
    expect_s4_class(res, "SelfHits")
    expect_equal(S4Vectors::nLnode(res), S4Vectors::nRnode(res))
    expect_equal(S4Vectors::nLnode(res), nrow(X))

    first <- res[S4Vectors::from(res) == 1]
    expect_identical(S4Vectors::to(first), out$index[1,])
    expect_identical(S4Vectors::mcols(first)$distance, out$distance[1,])

    last <- res[S4Vectors::from(res) == N]
    expect_identical(S4Vectors::to(last), out$index[N,])
    expect_identical(S4Vectors::mcols(last)$distance, out$distance[N,])
})
