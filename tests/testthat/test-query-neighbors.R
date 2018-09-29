# Tests queryNeighbors().
# library(BiocNeighbors); library(testthat); source("test-query-neighbors.R")

REINFORCE <- function(out) {
    O <- lapply(out$index, order)
    re.index <- mapply(FUN="[", x=out$index, i=O, SIMPLIFY=FALSE)
    re.dist <- mapply(FUN="[", x=out$distance, i=O, SIMPLIFY=FALSE)
    list(index=re.index, distance=re.dist)
}

expect_identical_re <- function(left, right) {
    expect_false(is.null(left$index))
    expect_false(is.null(right$index))
    expect_false(is.null(left$distance))
    expect_false(is.null(right$distance))
    
    L <- REINFORCE(left)
    R <- REINFORCE(right)
    expect_identical(L$index, R$index)
    expect_equal(L$distance, R$distance)
}

set.seed(1001)
test_that("queryNeighbors() behaves correctly with queries", {
    ndata <- 1000
    nquery <- 100

    for (ndim in c(1, 5, 10, 20)) {
        for (d in c(0.1, 0.5, 1)) { 
            X <- matrix(runif(ndata * ndim), nrow=ndata)
            Y <- matrix(runif(nquery * ndim), nrow=nquery)
            out <- queryNeighbors(X, threshold=d, query=Y)

            out.dist <- out.indx <- vector("list", nquery)
            for (j in seq_len(nquery)) {
                targets <- sqrt(colSums((Y[j,] - t(X))^2))
                chosen <- targets <= d
                out.indx[[j]] <- which(chosen)
                out.dist[[j]] <- targets[chosen]
            }

            expect_identical_re(out, list(index=out.indx, distance=out.dist))
        }
    }
})

set.seed(1002)
test_that("queryNeighbors() works correctly with subsetting", {
    nobs <- 1000
	nquery <- 93
    ndim <- 21
    d <- 1.5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
	Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- queryNeighbors(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.

    i <- sample(nquery, 20)
    sub <- queryNeighbors(X, Y, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    i <- rbinom(nquery, 1, 0.5) == 0L
    sub <- queryNeighbors(X, Y, threshold=d, subset=i)
    expect_identical_re(sub, lapply(ref, "[", i=i))

    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 50)
    sub <- queryNeighbors(X, Y, threshold=d, subset=i)
    m <- match(i, rownames(Y))
    expect_identical_re(sub, lapply(ref, "[", i=m))
})

set.seed(1003)
test_that("queryNeighbors() behaves correctly with alternative options", {
    nobs <- 1000
    nquery <- 100
    ndim <- 10
    d <- 1

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- queryNeighbors(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
    
    # Checking what we extract.
    out2 <- queryNeighbors(X, Y, threshold=d, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(lapply(out2$index, sort), lapply(ref$index, sort))

    out3 <- queryNeighbors(X, Y, threshold=d, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_equal(lapply(out3$distance, sort), lapply(ref$distance, sort))
  
    # Checking precomputation.
    pre <- buildKmknn(X)
    out4 <- queryNeighbors(query=Y, threshold=d, precomputed=pre) # no need for X!
    expect_identical_re(out4, ref)

    # Checking transposition.
    out5 <- queryNeighbors(X, threshold=d, query=t(Y), transposed=TRUE)
    expect_identical_re(out5, ref)
})

set.seed(100301)
test_that("queryNeighbors() behaves correctly with parallelization", {
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    d <- 1.5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    ref <- queryNeighbors(X, Y, threshold=d)
    expect_true(length(unique(lengths(ref$index))) > 1L) # some variety; not all, not single.
  
    # Trying out different types of parallelization.
    out1 <- queryNeighbors(X, Y, threshold=d, BPPARAM=MulticoreParam(2))
    expect_identical_re(out1, ref)

    out2 <- queryNeighbors(X, Y, threshold=d, BPPARAM=SnowParam(3))
    expect_identical_re(out2, ref)
})

set.seed(10031)
test_that("queryNeighbors() raw output behaves correctly", {
    nobs <- 1001
    nquery <- 101
    ndim <- 11
    d <- 1.5
    X <- matrix(runif(nobs * ndim), nrow=nobs)
  	Y <- matrix(runif(nquery * ndim), nrow=nquery)
 
    pre <- buildKmknn(X)
    out <- queryNeighbors(query=Y, threshold=d, precomputed=pre, raw.index=TRUE)
    ref <- queryNeighbors(query=Y, X=t(KmknnIndex_clustered_data(pre)), threshold=d)
    expect_identical_re(out, ref)

    # Behaves with subsetting.
    i <- sample(nquery, 20)
    out <- queryNeighbors(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- queryNeighbors(query=Y, X=t(KmknnIndex_clustered_data(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    i <- rbinom(nquery, 1, 0.5) == 0L
    out <- queryNeighbors(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- queryNeighbors(query=Y, X=t(KmknnIndex_clustered_data(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)

    # Adding row names.
    rownames(Y) <- paste0("CELL", seq_len(nquery))
    i <- sample(rownames(Y), 30)
    out <- queryNeighbors(query=Y, threshold=d, precomputed=pre, raw.index=TRUE, subset=i)
    ref <- queryNeighbors(query=Y, X=t(KmknnIndex_clustered_data(pre)), threshold=d, subset=i)
    expect_identical_re(out, ref)
})

set.seed(1004)
test_that("queryNeighbors() behaves correctly with silly inputs", {
    nobs <- 1000
	nquery <- 100
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    Y <- matrix(runif(nquery * ndim), nrow=nquery)
    
    # What happens when the threshold is not positive.
    expect_error(queryNeighbors(X, Y, threshold=0), "positive")
    expect_error(queryNeighbors(X, Y, threshold=-1), "positive")

    # What happens when there are no points in the data.
    out <- queryNeighbors(X[0,,drop=FALSE], Y, threshold=1)
    expect_identical(lengths(out$index), integer(nquery))
    expect_identical(lengths(out$distance), integer(nquery))

    # What happens when there are no points in the query.
    out <- queryNeighbors(X, Y[0,,drop=FALSE], threshold=1)
    expect_identical(out$index, list())
    expect_identical(out$distance, list())

    # What happens when there are no dimensions.
    out <- queryNeighbors(X[,0], Y[,0], threshold=1)
    expect_identical(unique(out$index), list(seq_len(nobs)))
    expect_identical(unique(out$distance), list(numeric(nobs)))

    # What happens when the query is of a different dimension.
    Z <- matrix(runif(nobs * ndim * 2), nrow=nobs)
    expect_error(queryNeighbors(X, threshold=1, query=Z), "dimensionality")

    # What happens when we request raw.index without precomputed.
    expect_error(queryNeighbors(X, Y, threshold=1, raw.index=TRUE), "not valid")

    # What happens when the query is not, strictly a matrix.
    AA <- data.frame(Y)
    colnames(AA) <- NULL
    expect_identical_re(queryNeighbors(X, Y, threshold=1), queryNeighbors(X, AA, threshold=1))
})
