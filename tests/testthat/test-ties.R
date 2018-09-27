# This tests the behaviour of findKmknn's C++ code for detecting ties.
# library(kmknn); library(testthat); source("test-ties.R")

test_that("ties within the set of nearest neighbors triggers errors", {
    # '1' and '3' are tied when compared to '2'.
    coordinates <- rbind(1,2,3)
    expect_warning(out <- findKmknn(coordinates, k=1), "tied distances detected")

    # Ties are now occurring _within_ the set.
    coordinates <- rbind(1,2,3)
    expect_warning(out <- findKmknn(coordinates, k=2), "tied distances detected")

    coordinates <- rbind(0.1, 1, 2, 3, 5.2)
    expect_warning(out <- findKmknn(coordinates, k=3), "tied distances detected")

    # No warning when there are no ties.
    coordinates <- rbind(1,2,4)
    expect_warning(out <- findKmknn(coordinates, k=1), NA)
    expect_warning(out <- findKmknn(coordinates, k=2), NA)

    coordinates <- rbind(0.1, 1, 2, 4, 4.1)
    expect_warning(out <- findKmknn(coordinates, k=3), NA)
})

MOCKUP <- function(coordinates) {
    info <- lapply(seq_len(nrow(coordinates)), FUN=function(i) {
        list(i-1L, 0)
    })

    KmknnIndex(data=t(coordinates), 
        centers=t(coordinates), info=info,
        order=seq_len(nrow(coordinates)),
        NAMES=NULL
    )
}

test_that("ties are correctly detected across clusters", {
    # Testing k=1 to check if 'last_distance' is correctly updated across clusters.
    # The second row should be equidistant to the two other clusters, 
    # so the use of TOLERANCE in the convex_holder::search_nn() C++ code should trigger.
    coordinates <- matrix(1:3, nrow=3, ncol=5)
    pre <- MOCKUP(coordinates)
    expect_warning(out <- findKmknn(precomputed=pre, k=1), "tied distances detected")

    # Tie breakage below the threshold of numerical precision.
    # Note that 1 becomes _closer_ to 2, while 3 moves further away.
    coordinates2 <- coordinates
    coordinates2[,1] <- coordinates2[,1] + c(1e-10, 0, 1e-10)
    pre2 <- MOCKUP(coordinates2)
    expect_warning(out <- findKmknn(precomputed=pre2, k=1), "tied distances detected")

    # Tie breakage above the threshold of numerical precision.
    coordinates2 <- coordinates
    coordinates2[,1] <- coordinates2[,1] + c(1e-5, 0, 1e-5)
    pre2 <- MOCKUP(coordinates2)
    expect_warning(out <- findKmknn(precomputed=pre2, k=2), NA)
})

test_that("ties are correctly detected at zero distance", {
    coordinates <- matrix(0, 10, 5)
    expect_warning(out <- findKmknn(coordinates, k=2), "tied distances detected")

    # Breaking the ties.
    coordinates[,1] <- runif(nrow(coordinates), 0, 1e-10)
    expect_warning(out <- findKmknn(coordinates, k=2), NA)

    # Testing with a less trivial example.        
    coordinates2 <- matrix(1:5, 10, 5, byrow=TRUE)
    expect_warning(out <- findKmknn(coordinates2, k=2), "tied distances detected")

    # Checking that zero distances across clusters are handled correctly.
    pre <- MOCKUP(coordinates2)
    expect_warning(out <- findKmknn(precomputed=pre, k=1), "tied distances detected")
})


