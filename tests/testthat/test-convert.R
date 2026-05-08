
test_that("coversion to sparseMatrix works", {
    X <- matrix(rnorm(10000), ncol = 20)
    out <- findKNN(X, k = 8)
    
    res <- convertToSparseMatrix(out)
  
    expect_s4_class(res, "sparseMatrix")
    expect_true(all(dim(res) == nrow(X)))
})

test_that("coversion to SelfHits works", {
    X <- matrix(rnorm(10000), ncol = 20)
    out <- findKNN(X, k = 8)
    
    res <- convertToSelfHits(out)
    expect_s4_class(res, "SelfHits")
    expect_equal(nLnode(res), nRnode(res))
    expect_equal(nLnode(res), nrow(X))
})
