# Tests buildAnnoy().
# library(BiocNeighbors); library(testthat); source("test-build-annoy.R")

set.seed(250000)
test_that("buildAnnoy() works as expected", {
    tmp.dir <- dirname(file.path(tempdir(), ".")) # convert Windows back slash to forward slash.

    for (ndim in c(1, 5, 10)) {
        for (nobs in c(500, 1000, 2000)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- buildAnnoy(X)
            expect_identical(dirname(AnnoyIndex_path(out)), tmp.dir)
            expect_identical(dim(out), dim(X))
        }
    }
})

set.seed(2500011)
test_that("buildAnnoy() respects path changes", {
    nobs <- 500
    ndim <- 5
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    out <- buildAnnoy(X, fname="whee.idx")
    expect_identical(AnnoyIndex_path(out), "whee.idx")
    out <- buildAnnoy(X, directory=".")
    expect_identical(dirname(AnnoyIndex_path(out)), ".")

    unlink(list.files(pattern="\\.idx$"))
})

set.seed(250001)
test_that("buildAnnoy() preserves dimension names", {
    nobs <- 1011
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    out <- buildAnnoy(X)
    expect_identical(rownames(out), rownames(X))

    # Still true if there are no cells.
    out <- buildAnnoy(X[0,,drop=FALSE])
    expect_identical(rownames(out), NULL)
})

set.seed(250002)
test_that("choice of 'ntrees' in buildAnnoy() has an effect", {
    nobs <- 2101
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    idx1 <- buildAnnoy(X, ntrees=10)
    out1 <- findAnnoy(precomputed=idx1, k=20)

    idx2 <- buildAnnoy(X, ntrees=100)
    out2 <- findAnnoy(precomputed=idx2, k=20)
    expect_false(identical(out1$index, out2$index))

    # As a control for randomness:
    idx3 <- buildAnnoy(X, ntrees=10)
    out3 <- findAnnoy(precomputed=idx3, k=20)
    expect_true(identical(out1$index, out3$index))
})

set.seed(250003)
test_that("buildAnnoy() behaves sensibly with silly inputs", {
    nobs <- 100L
    ndim <- 10L
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # What happens when there are no cells.
    out <- buildAnnoy(X[0,,drop=FALSE])
    expect_identical(dim(out), c(0L, ndim))

    # What happens when there are no dimensions.
    out <- buildAnnoy(X[,0,drop=FALSE])
    expect_identical(dim(out), c(nobs, 0L))

    # Checking that it behaves without distinct data points.
    expect_error(prec <- buildAnnoy(matrix(0, 10,10)), NA)

    # We get a sane result when 'X' is not, strictly, a matrix.
    Y <- data.frame(X, check.names=FALSE, fix.empty.names=FALSE)
    colnames(Y) <- NULL
    expect_error(out <- buildAnnoy(Y), NA)
})
