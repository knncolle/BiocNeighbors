# Tests findNeighbors().
# library(kmknn); library(testthat); source("test-findNeighbors.R")

set.seed(1001)
test_that("findNeighbors() behaves correctly on simple inputs", {
    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (d in c(0.1, 0.5, 1)) {
        library(kmknn); ndim <- 1; d <- 1; nobs <- 1000
            X <- matrix(runif(nobs * ndim), nrow=nobs)
    
            D <- unname(as.matrix(dist(X)))
            ind <- which(D <= d, arr.ind=TRUE)
            by.row <- split(ind[,2], ind[,1])

            out <- findNeighbors(X, threshold=d)
            expect_identical(length(by.row), length(out[[1]]))

            for (i in seq_along(by.row)) {
                ref.i <- by.row[[i]]
                ref.d <- D[i,ref.i]
                ref.o <- order(ref.i)

                obs.i <- out$index[[i]]
                obs.d <- out$distance[[i]]
                obs.o <- order(obs.i)

                expect_identical(ref.i[ref.o], obs.i[obs.o])                
                expect_identical(ref.d[ref.o], obs.d[obs.o])                
            }
        }
    }
})

set.seed(1002)
test_that("findNeighbors() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # Remember that the output indices are unordered, though the identities are constant.
    # Thus, we need to set the seed to get the same result.
    set.seed(123) 
    ref <- findNeighbors(X, threshold=d)

    i <- sample(nobs, 20)
    set.seed(123)
    sub <- findNeighbors(X, threshold=d, subset=i)
    expect_identical(sub$index, ref$index[i])
    expect_identical(sub$distance, ref$distance[i])

    i <- rbinom(nobs, 1, 0.5) == 0L
    set.seed(123)
    sub <- findNeighbors(X, threshold=d, subset=i)
    expect_identical(sub$index, ref$index[i])
    expect_identical(sub$distance, ref$distance[i])

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 123)
    set.seed(123)
    sub <- findNeighbors(X, threshold=d, subset=i)
    m <- match(i, rownames(X))
    expect_identical(sub$index, ref$index[m])
    expect_identical(sub$distance, ref$distance[m])
})

set.seed(1003)
test_that("findNeighbors() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    d <- 1
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    set.seed(234)
    out <- findNeighbors(X, threshold=d)
    
    # Checking what we extract.
    set.seed(234)
    out2 <- findNeighbors(X, threshold=d, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    set.seed(234)
    out3 <- findNeighbors(X, threshold=d, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation.
    set.seed(234)
    pre <- precluster(X)
    out4 <- findNeighbors(X, threshold=d, precomputed=pre)
    expect_identical(out4, out)
})

set.seed(1004)
test_that("findNeighbors() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(findNeighbors(X, threshold=0), "positive")
    expect_error(findNeighbors(X, threshold=-1), "positive")

    # What happens when there are no points.
    out <- findNeighbors(X[0,], threshold=1)
    expect_equal(out$index, list())
    expect_equal(out$distance, list())

    # What happens when there are no dimensions.
    out <- findNeighbors(X[,0], threshold=1)
    expect_identical(unique(out$index), list(seq_len(nobs)))
    expect_identical(unique(out$distance), list(numeric(nobs)))
})
