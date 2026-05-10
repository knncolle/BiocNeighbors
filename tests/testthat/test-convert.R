# library(testthat); library(BiocNeighbors); source("test-convert.R")

set.seed(99999)

test_that("findKNN to matrix works correctly", {
    X <- matrix(rnorm(10000), ncol = 20)
    N <- nrow(X)
    out <- findKNN(X, k = 8)

    res <- findResultsToMatrix(out)
    expect_s4_class(res, "dgCMatrix")
    expect_identical(nrow(res), N)
    expect_identical(ncol(res), N)

    for (i in c(1, 100, N)) {
        expected <- numeric(N)
        expected [out$index[i,]] <- out$distance[i,]
        expect_identical(expected, res[i,])
    }

    # Works with transposed input.
    {
        tout <- findKNN(X, k = 8, get.index="transposed")
        tres <- findResultsToMatrix(tout, index.transposed=TRUE)
        expect_identical(res, tres)

        tout <- findKNN(X, k = 8, get.distance="transposed")
        tres <- findResultsToMatrix(tout, distance.transposed=TRUE)
        expect_identical(res, tres)
    }

    # Works with transposed output.
    {
        tres <- findResultsToMatrix(out, query.rows = FALSE)
        expect_identical(res, Matrix::t(tres))
    }

    # Works with without distances.
    {
        out2 <- out
        out2$distance <- NULL
        lres <- findResultsToMatrix(out2)
        expect_s4_class(lres, "ngCMatrix")
        for (i in c(5, 20, 500)) {
            expect_identical(which(lres[i,]), sort(out$index[i,]))
        }
    }

    # Works with variable numbers of neighbors.
    {
        vout <- findKNN(X, k = sample(20, nrow(X), replace=TRUE))
        vres <- findResultsToMatrix(vout)

        for (i in c(2, 10, 50, 200)) {
            expected <- numeric(N)
            expected[vout$index[[i]]] <- vout$distance[[i]]
            expect_identical(expected, vres[i,])
        }
    }

    # Works if a subset is specified.
    {
        chosen <- rbinom(nrow(X), 1, 0.5) == 1
        sout <- findKNN(X, k = 8, subset = chosen)
        sres <- findResultsToMatrix(sout, num.pts = nrow(X))
        expect_identical(nrow(sres), sum(chosen))
        expect_identical(ncol(sres), N)
        expect_identical(sres, res[chosen,,drop=FALSE])
    }
})

test_that("findKNN to hits works correctly", {
    X <- matrix(rnorm(10000), ncol = 20)
    N <- nrow(X)
    out <- findKNN(X, k = 8)

    res <- findResultsToHits(out)
    expect_s4_class(res, "SelfHits")
    expect_equal(S4Vectors::nLnode(res), S4Vectors::nRnode(res))
    expect_equal(S4Vectors::nLnode(res), nrow(X))

    for (i in c(1, 2, 4, 8, 16, 32)) {
        expected <- res[S4Vectors::from(res) == i]
        expect_identical(S4Vectors::to(expected), out$index[i,])
        expect_identical(S4Vectors::mcols(expected)$distance, out$distance[i,])
    }

    # Works with transposed output.
    {
        tres <- findResultsToHits(out, query.from = FALSE)
        expect_identical(S4Vectors::from(res), S4Vectors::to(tres))
        expect_identical(S4Vectors::to(res), S4Vectors::from(tres))
        expect_identical(S4Vectors::mcols(res)$distance, S4Vectors::mcols(tres)$distance)
    }

    # Works with without distances.
    {
        out2 <- out
        out2$distance <- NULL
        lres <- findResultsToHits(out2)
        expect_identical(S4Vectors::from(res), S4Vectors::from(lres))
        expect_identical(S4Vectors::to(res), S4Vectors::to(lres))
        expect_null(S4Vectors::mcols(lres)$distance)
    }

    # Works if a subset is specified.
    {
        chosen <- rbinom(nrow(X), 1, 0.5) == 1
        sout <- findKNN(X, k = 8, subset = chosen)
        sres <- findResultsToHits(sout, num.pts = nrow(X))

        expect_s4_class(sres, "Hits")
        expect_identical(S4Vectors::nLnode(sres), sum(chosen)) 
        expect_identical(S4Vectors::nRnode(sres), nrow(X))

        for (i in round(c(N/2, N/8, N/16))) {
            expected <- res[S4Vectors::from(res) == i]
            expect_identical(S4Vectors::to(expected), out$index[i,])
            expect_identical(S4Vectors::mcols(expected)$distance, out$distance[i,])
        }
    }
})

# Most of the same machinery is reused from the findKNN tests, so we'll just do
# some cursory checks for the remaining functions.

test_that("findNeighbors conversions work correctly", {
    X <- matrix(rnorm(8000), ncol = 20)
    N <- nrow(X)
    out <- findNeighbors(X, threshold = 4)

    mat <- findResultsToMatrix(out)
    for (i in c(10, 100, 20, 50)) {
        expected <- numeric(N)
        expected[out$index[[i]]] <- out$distance[[i]]
        expect_identical(expected, mat[i,])
    }

    hits <- findResultsToHits(out)
    for (i in c(1, 2, 4, 8, 16, 32)) {
        expected <- hits[S4Vectors::from(hits) == i]
        expect_identical(S4Vectors::to(expected), out$index[[i]])
        expect_identical(S4Vectors::mcols(expected)$distance, out$distance[[i]])
    }
})

test_that("queryKNN conversions work correctly", {
    X <- matrix(rnorm(8000), ncol = 20)
    Y <- matrix(rnorm(2000), ncol=20)
    N <- nrow(X)
    out <- queryKNN(X=X, query=Y, k = 10)

    mat <- queryResultsToMatrix(out, nrow(X))
    expect_identical(dim(mat), c(nrow(Y), nrow(X)))
    for (i in c(9, 19, 99)) {
        expected <- numeric(N)
        expected[out$index[i,]] <- out$distance[i,]
        expect_identical(expected, mat[i,])
    }

    hits <- queryResultsToHits(out, nrow(X))
    expect_identical(S4Vectors::nLnode(hits), nrow(Y))
    expect_identical(S4Vectors::nRnode(hits), nrow(X))
    for (i in c(8, 18, 80)) {
        expected <- hits[S4Vectors::from(hits) == i]
        expect_identical(S4Vectors::to(expected), out$index[i,])
        expect_identical(S4Vectors::mcols(expected)$distance, out$distance[i,])
    }
})

test_that("queryNeighbors conversions work correctly", {
    X <- matrix(rnorm(8000), ncol = 20)
    N <- nrow(X)
    Y <- matrix(rnorm(2000), ncol=20)
    out <- queryNeighbors(X=X, query=Y, threshold = 4)

    mat <- queryResultsToMatrix(out, nrow(X))
    for (i in c(9, 19, 29, 79)) {
        expected <- numeric(N)
        expected[out$index[[i]]] <- out$distance[[i]]
        expect_identical(expected, mat[i,])
    }

    hits <- queryResultsToHits(out, nrow(X))
    for (i in c(10, 20, 40, 80)) {
        expected <- hits[S4Vectors::from(hits) == i]
        expect_identical(S4Vectors::to(expected), out$index[[i]])
        expect_identical(S4Vectors::mcols(expected)$distance, out$distance[[i]])
    }
})
