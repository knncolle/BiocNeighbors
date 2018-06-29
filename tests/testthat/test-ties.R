# This tests the behaviour of findKNN's C++ code for detecting ties.
# library(kmknn); library(testthat); source("test-ties.R")

test_that("ties within the set of nearest neighbors triggers errors", {
    # '1' and '3' are tied when compared to '2'.
    coordinates <- rbind(1,2,3)
    expect_warning(out <- findKNN(coordinates, k=1), "tied distances detected")

    # Ties are now occurring _within_ the set.
    coordinates <- rbind(1,2,3)
    expect_warning(out <- findKNN(coordinates, k=2), "tied distances detected")

    coordinates <- rbind(0.1, 1, 2, 3, 5.2)
    expect_warning(out <- findKNN(coordinates, k=3), "tied distances detected")

    # No warning when there are no ties.
    coordinates <- rbind(1,2,4)
    expect_warning(out <- findKNN(coordinates, k=1), NA)
    expect_warning(out <- findKNN(coordinates, k=2), NA)

    coordinates <- rbind(0.1, 1,2,4, 4.1)
    expect_warning(out <- findKNN(coordinates, k=3), NA)
})
