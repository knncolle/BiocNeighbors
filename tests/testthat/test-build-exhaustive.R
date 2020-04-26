# Tests buildExhaustive().
# library(BiocNeighbors); library(testthat); source("test-build-kmknn.R")

set.seed(20000)
test_that("buildExhaustive() works as expected", {
    for (ndim in c(1, 5, 10, 20)) {
        for (nobs in c(500, 1000, 2000)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            
            out <- buildExhaustive(X)
            expect_identical(dim(out), dim(X))
            expect_identical(rev(dim(bndata(out))), dim(X))
            expect_identical(sort(bnorder(out)), seq_len(nobs))
            expect_identical(bndata(out), t(X[bnorder(out),]))
            expect_identical(bndistance(out), "Euclidean")

        }
    }
})

set.seed(200001)
test_that("buildExhaustive() preserves dimension names", {
    nobs <- 1011
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    out <- buildExhaustive(X)
    expect_identical(rownames(out), rownames(X))
    expect_identical(rownames(bndata(out)), colnames(X))
    expect_identical(colnames(bndata(out)), rownames(X)[bnorder(out)])

    # Still true if there are no cells.
    out <- buildExhaustive(X[0,,drop=FALSE])
    expect_identical(rownames(bndata(out)), colnames(X))
    expect_identical(colnames(bndata(out)), NULL)
    expect_identical(rownames(out), NULL)
})

set.seed(200002)
test_that("buildExhaustive() responds to transposition", {
    nobs <- 1011
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    set.seed(101)
    ref <- buildExhaustive(X)
    set.seed(101)
    out <- buildExhaustive(t(X), transposed=TRUE)
    expect_identical(ref, out)

    # Check it works in a function.
    ref <- findExhaustive(X, k=5)
    out <- findExhaustive(t(X), k=5, transposed=TRUE)
    expect_identical(ref, out)
})

set.seed(200003)
test_that("buildExhaustive() works with the Manhattan distance", {
    nobs <- 1011
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    set.seed(102)
    ref <- buildExhaustive(X, distance="Manhattan")
    expect_identical(bndistance(ref), "Manhattan")

    res <- findExhaustive(precomputed=ref, k=5)
    val <- findExhaustive(X, k=5, distance="Manhattan")
    expect_identical(res, val)
})
 
set.seed(20001)
test_that("buildExhaustive() behaves sensibly with silly inputs", {
    nobs <- 100L
    ndim <- 10L
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # What happens when there are no cells.
    out <- buildExhaustive(X[0,,drop=FALSE])
    expect_identical(dim(bndata(out)), c(ndim, 0L))
    expect_identical(length(bnorder(out)), 0L)

    # What happens when there are no dimensions.
    out <- buildExhaustive(X[,0,drop=FALSE])
    expect_identical(dim(bndata(out)), c(0L, nobs))
    expect_identical(bnorder(out), seq_len(nobs))

    # Checking that it behaves without distinct data points.
    expect_error(prec <- buildExhaustive(matrix(0, 10,10)), NA)

    # We get the same result when 'X' is not, strictly, a matrix.
    set.seed(1999)
    ref <- buildExhaustive(X)
    set.seed(1999)
    Y <- data.frame(X, check.names=FALSE, fix.empty.names=FALSE)
    colnames(Y) <- NULL
    out <- buildExhaustive(Y)
    expect_equal(ref, out) # TODO: NULL attributes in dimnames.
})
