# library(testthat); library(BiocNeighbors); source("setup.R"); source("test-findKNN.R")

set.seed(999999)

test_that("findKNN works with basic options", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)
    ref <- refFindKNN(Y, k=8)
    expect_equal(out, ref)

    out <- findKNN(Y, k=8, BNPARAM=KmknnParam(distance="Manhattan"))
    ref <- refFindKNN(Y, k=8, type="manhattan")
    expect_equal(out, ref)

    out <- findKNN(Y, k=8, BNPARAM=KmknnParam(distance="Cosine"))
    Y1 <- Y/sqrt(rowSums(Y^2))
    ref <- findKNN(Y1, k=8)
    expect_equal(out, ref)
})

test_that("findKNN works in parallel", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)
    pout <- findKNN(Y, k=8, num.threads=2)
    expect_identical(out, pout)

    pout <- findKNN(Y, k=8, BPPARAM=BiocParallel::SnowParam(2))
    expect_identical(out, pout)
})

test_that("findKNN works with subsets", {
    Y <- matrix(rnorm(10000), ncol=20)
    full <- findKNN(Y, k=8)

    sout <- findKNN(Y, subset=1:10, k=8)
    out <- full
    out$index <- out$index[1:10,]
    out$distance <- out$distance[1:10,]
    expect_identical(out, sout)

    expect_error(findKNN(Y, k=8, subset=100000), "out-of-range")
    expect_warning(out <- findKNN(Y[0,,drop=FALSE], k=8), "capped")
    expect_identical(ncol(out$index), 0L)
    expect_identical(ncol(out$distance), 0L)

    # Works with non-integer options.
    keep <- rbinom(nrow(Y), 1, 0.5) == 1
    sout <- findKNN(Y, subset=keep, k=8)
    out <- full
    out$index <- out$index[keep,]
    out$distance <- out$distance[keep,]
    expect_identical(out, sout)

    Z <- Y
    rownames(Z) <- seq_len(nrow(Z))
    keep <- sample(rownames(Z), nrow(Z) / 2)
    sout <- findKNN(Z, subset=keep, k=8)
    out <- full
    out$index <- out$index[as.integer(keep),]
    out$distance <- out$distance[as.integer(keep),]
    expect_identical(out, sout)
})

test_that("findKNN works with variable k", {
    Y <- matrix(rnorm(10000), ncol=20)

    k <- rep(c(4, 10), length.out=nrow(Y))
    out <- findKNN(Y, k=k)

    keep <- k == 4
    ref <- findKNN(Y, k=4)
    expect_identical(do.call(rbind, out$index[keep]), ref$index[keep,])
    expect_identical(do.call(rbind, out$distance[keep]), ref$distance[keep,])

    keep <- k == 10
    ref <- findKNN(Y, k=10)
    expect_identical(do.call(rbind, out$index[keep]), ref$index[keep,])
    expect_identical(do.call(rbind, out$distance[keep]), ref$distance[keep,])

    # The AsIs forced variable works.
    out <- findKNN(Y, k=I(10), subset=1)
    ref <- findKNN(Y, k=10, subset=1)
    expect_identical(out$index[[1]], ref$index[1,])
    expect_identical(out$distance[[1]], ref$distance[1,])

    expect_error(findKNN(Y, k=1:10), "must be equal")
})

test_that("findKNN works with prebuilt indices", {
    Y <- matrix(rnorm(10000), ncol=20)

    built <- buildIndex(Y, k=8)
    out <- findKNN(Y, k=8)
    preout <- findKNN(built, k=8)
    expect_identical(out, preout)

    # Unaffected by BNPARAM settings at this point.
    preout <- findKNN(built, k=8, BNPARAM=AnnoyParam())
    expect_identical(out, preout)

    # Throws an error for deserialized prebuilt indices.
    tmp <- tempfile(fileext=".rds")
    saveRDS(built, tmp)
    expect_error(findKNN(readRDS(tmp), k=1), "null pointer")
})

test_that("findKNN works with variable outputs", {
    Y <- matrix(rnorm(10000), ncol=20)

    out <- findKNN(Y, k=8)

    iout <- findKNN(Y, k=8, get.distance=FALSE)
    expect_null(iout$distance)
    expect_identical(iout$index, out$index)

    dout <- findKNN(Y, k=8, get.index=FALSE)
    expect_null(dout$index)
    expect_identical(dout$distance, out$distance)

    tout <- findKNN(Y, k=8, get.index="transposed", get.distance="transposed")
    expect_identical(t(tout$distance), out$distance)
    expect_identical(t(tout$index), out$index)
})
