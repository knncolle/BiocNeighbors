# Tests precluster().
# library(kmknn); library(testthat); source("test-pre.R")

test_that("precluster() works as expected", {
    for (ndim in c(1, 5, 10, 20)) {
        for (nobs in c(500, 1000, 2000)) { 
            X <- matrix(runif(nobs * ndim), nrow=nobs)
            
            out <- precluster(X)
            expect_identical(rev(dim(out$X)), dim(X))
            expect_identical(sort(out$order), seq_len(nobs))
            expect_identical(out$X, t(X[out$order,]))

            accounted <- logical(nobs)
            for (i in seq_along(out$clusters$info)) {
                current <- out$clusters$info[[i]]
                expect_false(is.unsorted(current[[2]]))

                idx <- current[[1]] + seq_along(current[[2]])
                expect_true(!any(accounted[idx]))
                accounted[idx] <- TRUE

                # Oddly inaccurate, for some reason...
                expect_equal(rowMeans(out$X[,idx,drop=FALSE]), unname(out$clusters$centers[,i]), tol=1e-4)
            }

            expect_true(all(accounted))
        }
    }
})

test_that("precluster() behaves sensibly with silly inputs", {
    nobs <- 100L
    ndim <- 10L
    X <- matrix(runif(nobs * ndim), nrow=nobs)

    # What happens when there are no cells.
    out <- precluster(X[0,,drop=FALSE])
    expect_identical(dim(out$X), c(ndim, 0L))
    expect_identical(dim(out$clusters$centers), c(ndim, 0L))
    expect_identical(length(out$clusters$info), 0L)
    expect_identical(length(out$order), 0L)

    # What happens when there are no dimensions.
    out <- precluster(X[,0,drop=FALSE])
    expect_identical(dim(out$X), c(0L, nobs))
    expect_identical(dim(out$clusters$centers), c(0L, 1L))
    expect_identical(length(out$clusters$info), 1L)
    expect_identical(out$clusters$info[[1]][[1]], 0L)
    expect_identical(out$clusters$info[[1]][[2]], numeric(nobs))
    expect_identical(out$order, seq_len(nobs))
})
