# Tests findAnnoy().
# library(BiocNeighbors); library(testthat); source("setup.R"); source("test-find-annoy.R")

skip("not attempting to compare to latest RcppAnnoy")

set.seed(7001)
test_that("findAnnoy() behaves correctly on simple inputs", {
    library(RcppAnnoy)
    REFFUN <- function(X, k, ntrees=50) {
        a <- new(AnnoyEuclidean, ncol(X))
        for (i in seq_len(nrow(X))) {
            a$addItem(i-1L, X[i,])
        }
        a$build(ntrees)
    
        collected.dex <- collected.dist <- vector("list", nrow(X))
        for (i in seq_len(nrow(X))) {
            available <- a$getNNsByItemList(i-1L, k+1, search_k=-1, include_distances=TRUE)
            
            # Ignore self, but otherwise take the top 'k'.
            available$item <- available$item + 1L
            keep <- available$item!=i
            available <- lapply(available, function(x) head(x[keep], k))

            collected.dex[[i]] <- available$item
            collected.dist[[i]] <- available$distance
        }
    
        list(index=do.call(rbind, collected.dex),
            distance=do.call(rbind, collected.dist))            
    }

    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- findAnnoy(X, k=k)
            ref <- REFFUN(X, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance)
        }
    }
})

set.seed(7002)
test_that("findAnnoy() works correctly with subsetting", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    ref <- findAnnoy(X, k=k)

    i <- sample(nobs, 20)
    sub <- findAnnoy(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    i <- rbinom(nobs, 1, 0.5) == 0L
    sub <- findAnnoy(X, k=k, subset=i)
    expect_identical(sub$index, ref$index[i,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[i,,drop=FALSE])

    rownames(X) <- paste0("CELL", seq_len(nobs))
    i <- sample(rownames(X), 100)
    sub <- findAnnoy(X, k=k, subset=i)
    m <- match(i, rownames(X))
    expect_identical(sub$index, ref$index[m,,drop=FALSE])
    expect_identical(sub$distance, ref$distance[m,,drop=FALSE])
})

set.seed(7003)
test_that("findAnnoy() behaves correctly with alternative options", {
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findAnnoy(X, k=k)
    
    # Checking what we extract.
    out2 <- findAnnoy(X, k=k, get.distance=FALSE)
    expect_identical(out2$distance, NULL)
    expect_identical(out2$index, out$index)

    out3 <- findAnnoy(X, k=k, get.index=FALSE)
    expect_identical(out3$index, NULL)
    expect_identical(out3$distance, out$distance)
  
    # Checking precomputation (does not need X).
    pre <- buildAnnoy(X)
    out4 <- findAnnoy(k=k, precomputed=pre)
    expect_identical(out4, out)
})

set.seed(70031)
test_that("findAnnoy() behaves correctly with Manhattan distances", {
    REFFUN <- function(X, k, ntrees=50) {
        a <- new(AnnoyManhattan, ncol(X))
        for (i in seq_len(nrow(X))) {
            a$addItem(i-1L, X[i,])
        }
        a$build(ntrees)
    
        collected.dex <- collected.dist <- vector("list", nrow(X))
        for (i in seq_len(nrow(X))) {
            available <- a$getNNsByItemList(i-1L, k+1, search_k=-1, include_distances=TRUE)

            available$item <- available$item + 1L
            keep <- available$item!=i
            available <- lapply(available, function(x) head(x[keep], k))

            collected.dex[[i]] <- available$item
            collected.dist[[i]] <- available$distance
        }
    
        list(index=do.call(rbind, collected.dex),
            distance=do.call(rbind, collected.dist))            
    }

    nobs <- 1000
    for (ndim in c(1, 5, 10, 20)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            out <- findAnnoy(X, k=k, distance="Manhattan")
            ref <- REFFUN(X, k=k)
            expect_identical(out$index, ref$index)
            expect_equal(out$distance, ref$distance) 
        }
    }
})

set.seed(700311)
test_that("findAnnoy() behaves correctly when only the last distance is requested", {
    nobs <- 500 
    for (ndim in c(1, 5, 10)) {
        for (k in c(1, 5, 20)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)

            ref <- findAnnoy(X, k=k)
            out <- findAnnoy(X, k=k, last=1)
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])

            ref <- findAnnoy(X, k=k, distance="Manhattan")
            out <- findAnnoy(X, k=k, last=1, distance="Manhattan")
            expect_identical(out$distance, ref$distance[,k,drop=FALSE])
            expect_identical(out$index, ref$index[,k,drop=FALSE])
        }
    }
})

set.seed(70032)
test_that("findAnnoy() responds to run-time search.k", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    k <- 7
    ref <- findAnnoy(X, k=k)
    alt <- findAnnoy(X, k=k, search.mult=20)
    expect_false(identical(alt$index, ref$index))

    # As a control:
    alt <- findAnnoy(X, k=k, search.mult=50)
    expect_true(identical(alt$index, ref$index))
})
 
set.seed(7004)
test_that("findAnnoy() behaves correctly with parallelization", {
    library(BiocParallel)
    nobs <- 1000
    ndim <- 10
    k <- 5

    X <- matrix(runif(nobs * ndim), nrow=nobs)
    out <- findAnnoy(X, k=k)
  
    # Trying out different types of parallelization.
    out1 <- findAnnoy(X, k=k, BPPARAM=safeBPParam(2))
    expect_identical(out$index, out1$index)
    expect_identical(out$distance, out1$distance)

    out2 <- findAnnoy(X, k=k, BPPARAM=SnowParam(3))
    expect_identical(out$index, out2$index)
    expect_identical(out$distance, out2$distance)
})

set.seed(7005)
test_that("findAnnoy() behaves correctly with silly inputs", {
    nobs <- 1000
    ndim <- 10
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    
    # What happens when k is not positive.
    expect_error(findAnnoy(X, k=0), "positive")
    expect_error(findAnnoy(X, k=-1), "positive")

    # What happens when 'k' > dataset size.
    restrict <- 10
    expect_warning(out <- findAnnoy(X[seq_len(restrict),], k=20), "capped")
    expect_warning(ref <- findAnnoy(X[seq_len(restrict),], k=restrict-1L), NA)
    expect_equal(out, ref)

    # What happens when there are no dimensions.
    out <- findAnnoy(X[,0], k=20)
    expect_identical(nrow(out$index), as.integer(nobs))
    expect_identical(ncol(out$index), 20L)
    expect_identical(dim(out$index), dim(out$distance))
    expect_true(all(out$distance==0))

    # What happens with nothing.
    expect_identical(findAnnoy(X, k=10, get.distance=FALSE, get.index=FALSE), list())
})
