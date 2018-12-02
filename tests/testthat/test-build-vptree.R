# Tests buildVptree().
# library(BiocNeighbors); library(testthat); source("test-build-vptree.R")

set.seed(20000)
test_that("buildVptree() works as expected", {
    for (ndim in c(1, 5, 10, 20)) {
        for (nobs in c(500, 1000, 2000)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            
            out <- buildVptree(X)
            expect_identical(dim(out), dim(X))
            expect_identical(rev(dim(VptreeIndex_data(out))), dim(X))
            expect_identical(sort(VptreeIndex_order(out)), seq_len(nobs))
            expect_identical(VptreeIndex_data(out), t(X[VptreeIndex_order(out),]))

            node.data <- VptreeIndex_nodes(out)
            expect_identical(sort(node.data[[1]]), seq_len(nobs)-1L)
            expect_true(!anyDuplicated(setdiff(node.data[[2]], -1L)))
            expect_true(!anyDuplicated(setdiff(node.data[[3]], -1L)))
            expect_true(all(node.data[[4]] >= 0))
        }
    }
})

set.seed(200001)
test_that("buildVptree() preserves dimension names", {
    nobs <- 1011
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    out <- buildVptree(X)
    expect_identical(rownames(out), rownames(X))
    expect_identical(rownames(KmknnIndex_clustered_data(out)), colnames(X))
    expect_identical(colnames(VptreeIndex_data(out)), rownames(X)[VptreeIndex_order(out)])

    # Still true if there are no cells.
    out <- buildVptree(X[0,,drop=FALSE])
    expect_identical(rownames(KmknnIndex_clustered_data(out)), colnames(X))
    expect_identical(colnames(VptreeIndex_data(out)), NULL)
    expect_identical(rownames(out), NULL)
})

set.seed(20001)
test_that("buildVptree() behaves sensibly with silly inputs", {
    nobs <- 100L
    ndim <- 10L
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # What happens when there are no cells.
    out <- buildVptree(X[0,,drop=FALSE])
    expect_identical(dim(VptreeIndex_data(out)), c(ndim, 0L))
    expect_identical(length(VptreeIndex_order(out)), 0L)
    expect_identical(lengths(VptreeIndex_nodes(out)), integer(4))

    # What happens when there are no dimensions.
    out <- buildVptree(X[,0,drop=FALSE])
    expect_identical(dim(VptreeIndex_data(out)), c(0L, nobs))
    expect_identical(sort(VptreeIndex_order(out)), seq_len(nobs)) # arbitrary ordering.
    expect_identical(lengths(VptreeIndex_nodes(out)), rep(nobs, 4))

    # Checking that it behaves without distinct data points.
    expect_error(prec <- buildVptree(matrix(0, 10,10)), NA)

    # We get the same result when 'X' is not, strictly, a matrix.
    set.seed(1999)
    ref <- buildVptree(X)
    set.seed(1999)
    Y <- data.frame(X, check.names=FALSE, fix.empty.names=FALSE)
    colnames(Y) <- NULL
    out <- buildVptree(Y)
    dimnames(out@data) <- NULL # fix not-quite-empty dimnames.
    expect_equal(ref, out)
})
