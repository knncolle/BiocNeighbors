# library(BiocNeighbors); library(testthat); source("test-findMutualNN.R")

set.seed(10000)
test_that("Mutual NN detection is correct", {
    # Reference NNs.
    REF <- function(d1, d2, k1, k2) {
        n1 <- nrow(d1)
        n2 <- nrow(d2)
        n.total <- n1 + n2
   
        nn21 <- BiocNeighbors::queryKNN(d2, query=d1, k=k2)
        nn12 <- BiocNeighbors::queryKNN(d1, query=d2, k=k1)

        all.pairs.1 <- all.pairs.2 <- vector("list", n1)

        for (i in seq_len(n1)) {
            neighbors <- nn21$index[i,]
            converse <- nn12$index[neighbors,,drop=FALSE]
            mutual <- rowSums(converse==i) > 0L
            all.pairs.1[[i]] <- rep(i, sum(mutual))
            all.pairs.2[[i]] <- neighbors[mutual]
        }

        list(first=unlist(all.pairs.1), second=unlist(all.pairs.2))
    }
    
    # Check that the values are identical.
    comparator <- function(x, y) {
        ox <- order(x$first, x$second)
        oy <- order(y$first, y$second)
        expect_identical(x$first[ox], y$first[oy])
        expect_identical(x$second[ox], y$second[oy]) 
    }
    
    # Compare to actual run.
    A <- matrix(rnorm(10000), ncol=50)
    B <- matrix(rnorm(20000), ncol=50)
    comparator(REF(A, B, 10, 10), findMutualNN(A, B, 10, 10))
    comparator(REF(A, B, 5, 20), findMutualNN(A, B, 5, 20))
    comparator(REF(A, B, 20, 5), findMutualNN(A, B, 20, 5))
    comparator(REF(A, B, 1, 1), findMutualNN(A, B, 1, 1))

    A <- matrix(rnorm(25000, 2), ncol=100)
    B <- matrix(rnorm(15000, 5), ncol=100)
    comparator(REF(A, B, 10, 10), findMutualNN(A, B, 10, 10))
    comparator(REF(A, B, 5, 20), findMutualNN(A, B, 5, 20))
    comparator(REF(A, B, 20, 5), findMutualNN(A, B, 20, 5))
    comparator(REF(A, B, 1, 1), findMutualNN(A, B, 1, 1))
})

set.seed(10000)
test_that("Mutual NN detection works with prebuilt indices", {
    A <- matrix(rnorm(10000), ncol=50)
    B <- matrix(rnorm(20000), ncol=50)
    ref <- findMutualNN(A, B, k1=10)

    B1 <- buildIndex(A)
    B2 <- buildIndex(B)
    expect_identical(ref, findMutualNN(A, B, k1=10, BNINDEX1=B1))
    expect_identical(ref, findMutualNN(A, B, k1=10, BNINDEX2=B2))
    expect_identical(ref, findMutualNN(A, B, k1=10, BNINDEX1=B1, BNINDEX2=B2))

    # Works for other methods.
    B1 <- buildIndex(A, BNPARAM=AnnoyParam())
    B2 <- buildIndex(B, BNPARAM=AnnoyParam())
    ref <- findMutualNN(A, B, k1=10, BNPARAM=AnnoyParam())

    expect_identical(ref, findMutualNN(A, B, k1=10, BNPARAM=AnnoyParam(), BNINDEX1=B1))
    expect_identical(ref, findMutualNN(A, B, k1=10, BNPARAM=AnnoyParam(), BNINDEX2=B2))
    expect_identical(ref, findMutualNN(A, B, k1=10, BNPARAM=AnnoyParam(), BNINDEX1=B1, BNINDEX2=B2))
})
