# Tests precluster().
# library(kmknn); library(testthat); source("test-precluster.R")

set.seed(20000)
test_that("precluster() works as expected", {
    for (ndim in c(1, 5, 10, 20)) {
        for (nobs in c(500, 1000, 2000)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            
            out <- precluster(X)
            expect_identical(rev(dim(out$data)), dim(X))
            expect_identical(sort(out$order), seq_len(nobs))
            expect_identical(out$data, t(X[out$order,]))

            Nclust <- length(out$clusters$info)
            expect_identical(Nclust, as.integer(ceiling(sqrt(nobs))))

            accounted <- logical(nobs)
            unsorted <- !logical(Nclust)
            collected <- vector("list", Nclust)

            for (i in seq_along(out$clusters$info)) {
                current <- out$clusters$info[[i]]
                unsorted[i] <- is.unsorted(current[[2]])

                idx <- current[[1]] + seq_along(current[[2]])
                expect_true(!any(accounted[idx]))
                accounted[idx] <- TRUE

                collected[[i]] <- rowMeans(out$data[,idx,drop=FALSE]) 
            }

            expect_true(all(accounted))
            expect_true(!any(unsorted))
            expect_equal(do.call(cbind, collected), unname(out$clusters$centers), tol=1e-4) # Oddly inaccurate, for some reason...
        }
    }
})

set.seed(200001)
test_that("precluster() preserves dimension names", {
    nobs <- 1011
    ndim <- 23
    X <- matrix(runif(nobs * ndim), nrow=nobs)
    rownames(X) <- paste0("POINT", seq_len(nobs))
    colnames(X) <- paste0("DIM", seq_len(ndim))

    out <- precluster(X)
    expect_identical(rownames(out$data), colnames(X))
    expect_identical(colnames(out$data), rownames(X)[out$order])

    # Still true if there are no cells.
    out <- precluster(X[0,,drop=FALSE])
    expect_identical(rownames(out$data), colnames(X))
    expect_identical(colnames(out$data), NULL)
})

set.seed(20001)
test_that("precluster() behaves sensibly with silly inputs", {
    nobs <- 100L
    ndim <- 10L
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # What happens when there are no cells.
    out <- precluster(X[0,,drop=FALSE])
    expect_identical(dim(out$data), c(ndim, 0L))
    expect_identical(dim(out$clusters$centers), c(ndim, 0L))
    expect_identical(length(out$clusters$info), 0L)
    expect_identical(length(out$order), 0L)

    # What happens when there are no dimensions.
    out <- precluster(X[,0,drop=FALSE])
    expect_identical(dim(out$data), c(0L, nobs))
    expect_identical(dim(out$clusters$centers), c(0L, 1L))
    expect_identical(length(out$clusters$info), 1L)
    expect_identical(out$clusters$info[[1]][[1]], 0L)
    expect_identical(out$clusters$info[[1]][[2]], numeric(nobs))
    expect_identical(out$order, seq_len(nobs))
})
