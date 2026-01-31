# library(testthat); library(BiocNeighbors); source("test-loadIndex.R")

test_that("saving and loading works for all of our algorithms", {
    Y <- matrix(rnorm(10000), ncol=20)
    algos <- list(vptree=VptreeParam(), kmknn=KmknnParam(), brute=ExhaustiveParam(), annoy=AnnoyParam(), hnsw=HnswParam())

    for (algo in names(algos)) {
        idx <- buildIndex(Y, BNPARAM=algos[[algo]])

        tmp <- tempfile()
        dir.create(tmp)
        saveIndex(idx, tmp)

        reloaded <- loadIndex(tmp)
        expect_identical(findKNN(idx, k=5), findKNN(reloaded, k=5))
    }
})

test_that("saving and loading respects the names", {
    Y <- matrix(rnorm(10000), ncol=20)
    rownames(Y) <- sprintf("SAMPLE_%s", seq_len(nrow(Y)))
    idx <- buildIndex(Y)

    tmp <- tempfile()
    dir.create(tmp)
    saveIndex(idx, tmp)

    reloaded <- loadIndex(tmp)
    expect_identical(reloaded@names, rownames(Y))
})

test_that("saving and loading works for cosine distances", {
    Y <- matrix(rnorm(10000), ncol=20)
    idx <- buildIndex(Y, VptreeParam(distance="Cosine"))

    tmp <- tempfile()
    dir.create(tmp)
    saveIndex(idx, tmp)

    reloaded <- loadIndex(tmp)
    expect_identical(findKNN(idx, k=10), findKNN(reloaded, k=10))
})

test_that("saving and loading works with custom loading functions", {
    tmp <- tempfile()
    dir.create(tmp)
    cat(file=file.path(tmp, "ALGORITHM"), "FOO")

    registerLoadIndexFunction("FOO", function(dir, ...) list(BAR=dir))
    reloaded <- loadIndex(tmp)
    expect_identical(reloaded$BAR, tmp)
})
