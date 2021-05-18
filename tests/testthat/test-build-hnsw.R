# Tests buildHnsw().
# library(BiocNeighbors); library(testthat); source("test-build-hnsw.R")

set.seed(250000)
test_that("buildHnsw() works as expected", {
    tmp.dir <- dirname(file.path(tempdir(), ".")) # convert Windows back slash to forward slash.

    for (ndim in c(1, 5, 10)) {
        for (nobs in c(500, 1000, 2000)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- buildHnsw(X)
            expect_identical(dirname(HnswIndex_path(out)), tmp.dir)
            expect_identical(dim(out), dim(X))
            expect_identical(bndistance(out), "Euclidean")
        }
    }
})

set.seed(2500001)
test_that("buildHnsw() respects path changes", {
    nobs <- 500
    ndim <- 5
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    out <- buildHnsw(X, fname="whee.idx")
    expect_identical(HnswIndex_path(out), "whee.idx")
    out <- buildHnsw(X, directory=".")
    expect_identical(dirname(HnswIndex_path(out)), ".")

    unlink(list.files(pattern="\\.idx$"))
})

set.seed(250001)
test_that("buildHnsw() preserves dimension names", {
    nobs <- 1011
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    out <- buildHnsw(X)
    expect_identical(rownames(out), rownames(X))

    # Still true if there are no cells.
    out <- buildHnsw(X[0,,drop=FALSE])
    expect_identical(rownames(out), NULL)
})

set.seed(2500011)
test_that("buildHnsw() responds to transposition", {
    nobs <- 1011
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    set.seed(101)
    ref <- buildHnsw(X)
    set.seed(101)
    out <- buildHnsw(t(X), transposed=TRUE)

    expect_identical(bndata(ref), bndata(out))
    expect_identical(rownames(ref), rownames(out))
    expect_identical(bnorder(ref), bnorder(out))

    # Check it works in a function.
    ref <- findHnsw(X, k=5)
    out <- findHnsw(t(X), k=5, transposed=TRUE)
    expect_identical(out, ref)
})

set.seed(250002)
test_that("choice of parameters in buildHnsw() has an effect", {
    nobs <- 2101
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    idx1 <- buildHnsw(X, nlinks=10)
    out1 <- findHnsw(precomputed=idx1, k=20)

    idx2 <- buildHnsw(X, nlinks=50)
    out2 <- findHnsw(precomputed=idx2, k=20)
    expect_false(identical(out1$index, out2$index))

    # As a control for randomness:
    idx3 <- buildHnsw(X, nlinks=10)
    out3 <- findHnsw(precomputed=idx3, k=20)
    expect_true(identical(out1$index, out3$index))
})

set.seed(250003)
test_that("choice of 'ef.search' in buildHnsw() has an effect", {
    nobs <- 2101
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    idx <- buildHnsw(X)
    expect_identical(HnswIndex_ef_search(idx), 10L)

    idx <- buildHnsw(X, ef.search=100)
    expect_identical(HnswIndex_ef_search(idx), 100L)
})

set.seed(2500021)
test_that("buildHnsw() works with the Manhattan distance", {
    nobs <- 1011
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    set.seed(102)
    ref <- buildHnsw(X, distance="Manhattan")
    expect_identical(bndistance(ref), "Manhattan")

    res <- findHnsw(precomputed=ref, k=5)
    val <- findHnsw(X, k=5, distance="Manhattan")
    expect_identical(res, val)
})

set.seed(2500022)
test_that("buildHnsw() works with the Cosine distance", {
    nobs <- 1011
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    set.seed(102)
    ref <- buildHnsw(X, distance="Cosine")
    expect_identical(bndistance(ref), "Cosine")

    res <- findHnsw(precomputed=ref, k=5)
    val <- findHnsw(X, k=5, distance="Cosine")
    expect_identical(res, val)
})

set.seed(250003)
test_that("buildHnsw() behaves sensibly with silly inputs", {
    nobs <- 100L
    ndim <- 10L
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # What happens when there are no cells.
    out <- buildHnsw(X[0,,drop=FALSE])
    expect_identical(dim(out), c(0L, ndim))

    # What happens when there are no dimensions.
    out <- buildHnsw(X[,0,drop=FALSE])
    expect_identical(dim(out), c(nobs, 0L))

    # Checking that it behaves without distinct data points.
    expect_error(prec <- buildHnsw(matrix(0, 10,10)), NA)

    # We get a sane result when 'X' is not, strictly, a matrix.
    Y <- data.frame(X, check.names=FALSE, fix.empty.names=FALSE)
    colnames(Y) <- NULL
    expect_error(out <- buildHnsw(Y), NA)
})
