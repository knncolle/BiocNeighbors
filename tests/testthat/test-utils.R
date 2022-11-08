# Tests the utilities.
# library(BiocNeighbors); library(testthat); source("test-utils.R")

library(BiocParallel)
set.seed(80000)
test_that("worker job assignment works correctly", {
    jobs <- sample(1000)
    by.core <- BiocNeighbors:::.assign_jobs(jobs, SerialParam())
    expect_identical(list(jobs), by.core)

    for (x in 2:10) { 
        P <- SnowParam(x)
        by.core <- BiocNeighbors:::.assign_jobs(jobs, P)
        expect_identical(length(by.core), bpnworkers(P))
        expect_identical(jobs, unlist(by.core))
    }

    # Empty inputs.
    jobs <- integer(0)
    by.core <- BiocNeighbors:::.assign_jobs(jobs, SerialParam())
    expect_identical(list(jobs), by.core)
    by.core <- BiocNeighbors:::.assign_jobs(jobs, SnowParam(2))
    expect_identical(list(integer(0), integer(0)), by.core)
})

set.seed(80001)
test_that("subset index conversion works correctly", {
    A <- matrix(0, 20, 10)
    rownames(A) <- paste0("WHEE", seq_len(nrow(A)))
    colnames(A) <- paste0("YAY", seq_len(ncol(A)))

    # Integer by row.
    i <- sample(nrow(A))
    expect_identical(i, BiocNeighbors:::.subset_to_index(i, A, byrow=TRUE))
    expect_error(BiocNeighbors:::.subset_to_index(i, A, byrow=FALSE), "out of range")
    expect_identical(integer(0), BiocNeighbors:::.subset_to_index(integer(0), A, byrow=TRUE))

    # Integer by column.
    i <- sample(ncol(A), 15, replace=TRUE)
    expect_identical(i, BiocNeighbors:::.subset_to_index(i, A, byrow=FALSE))
    expect_identical(integer(0), BiocNeighbors:::.subset_to_index(integer(0), A, byrow=FALSE))

    # Logical by row
    j <- rbinom(nrow(A), 1, 0.5)==1
    expect_identical(which(j), BiocNeighbors:::.subset_to_index(j, A, byrow=TRUE))
    expect_error(BiocNeighbors:::.subset_to_index(j, A, byrow=FALSE), "out of range")
    expect_identical(integer(0), BiocNeighbors:::.subset_to_index(logical(0), A, byrow=TRUE))

    # Logical by column.
    j <- rbinom(ncol(A), 1, 0.5)==1
    expect_identical(which(j), BiocNeighbors:::.subset_to_index(j, A, byrow=FALSE))
    expect_identical(integer(0), BiocNeighbors:::.subset_to_index(logical(0), A, byrow=FALSE))

    # Character by row.
    k <- sample(rownames(A), 32, replace=TRUE)
    expect_identical(match(k, rownames(A)), BiocNeighbors:::.subset_to_index(k, A, byrow=TRUE))
    expect_error(BiocNeighbors:::.subset_to_index(k, A, byrow=FALSE), "out of range")
    expect_identical(integer(0), BiocNeighbors:::.subset_to_index(character(0), A, byrow=TRUE))

    # Character by column.
    k <- sample(colnames(A), 12, replace=TRUE)
    expect_identical(match(k, colnames(A)), BiocNeighbors:::.subset_to_index(k, A, byrow=FALSE))
    expect_error(BiocNeighbors:::.subset_to_index(k, A, byrow=TRUE), "out of range")
    expect_identical(integer(0), BiocNeighbors:::.subset_to_index(character(0), A, byrow=FALSE))
})

test_that("l2norm computes itself correctly", {
    A <- matrix(rnorm(1000), ncol=10)
    X <- BiocNeighbors:::l2norm(A, transposed=FALSE)    
    expect_true(all(abs(rowSums(X^2) - 1) < 1e-8))

    Y <- BiocNeighbors:::l2norm(t(A))
    expect_identical(X, t(Y))

    # Checking the theory of the calculations by comparing to Pearson's correlation.
    B <- A - rowMeans(A)
    Z <- BiocNeighbors:::l2norm(B, transposed=FALSE)    
    out <- sqrt(sum((Z[1,] - Z[2,])^2))
    ref <- sqrt(0.5 * (1 - cor(A[1,], A[2,])))
    expect_equal(out/2, ref)
})
