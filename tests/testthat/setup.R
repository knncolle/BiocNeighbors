#################################################
# Euclidean function for finding all neighbors in range.

refFindNeighbors <- function(X, threshold, type="euclidean") {
    D <- unname(as.matrix(dist(X, method=type)))
    ind <- which(D <= threshold, arr.ind=TRUE)
    by.row <- split(ind[,2], ind[,1])
    by.dist <- split(D[ind], ind[,1])
    list(index=unname(by.row), distance=unname(by.dist))
}

refQueryNeighbors <- function(X, Y, threshold, type="euclidean") {
    out.dist <- out.indx <- vector("list", nrow(Y))
    for (j in seq_len(nrow(Y))) {
        diff <- Y[j,] - t(X)
        if (type=="euclidean") {
            targets <- sqrt(colSums(diff^2))
        } else {
            targets <- colSums(abs(diff))
        }

        chosen <- targets <= threshold
        out.indx[[j]] <- which(chosen)
        out.dist[[j]] <- targets[chosen]
    }
    list(index=out.indx, distance=out.dist)
}

#################################################
# Setting up some common functions for range find checks.

REINFORCE <- function(out) {
# Remember that the output indices are unordered, though the identities are constant.
# Thus, we need to do some work to ensure that we get the same result.
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

#################################################
# Setting up some common function for Manhattan distances.

findKNN.L1.EXACT <- function(X, k) {
    collected.index <- collected.dist <- list()
    tX <- t(X)
    for (i in seq_len(nrow(X))) {
        all.dist <- colSums(abs(tX - X[i,]))
        o <- order(all.dist)
        keep <- head(setdiff(o, i), k)
        collected.index[[i]] <- keep
        collected.dist[[i]] <- all.dist[keep]
    }
    
    list(index=do.call(rbind, collected.index),
        distance=do.call(rbind, collected.dist))
}

queryKNN.L1.EXACT <- function(X, Y, k) {
    collected.index <- collected.dist <- list()
    tX <- t(X)
    for (i in seq_len(nrow(Y))) {
        all.dist <- colSums(abs(tX - Y[i,]))
        o <- order(all.dist)
        keep <- head(o, k)
        collected.index[[i]] <- keep
        collected.dist[[i]] <- all.dist[keep]
    }
    
    list(index=do.call(rbind, collected.index),
        distance=do.call(rbind, collected.dist))
}
