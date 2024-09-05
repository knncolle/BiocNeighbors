#################################################
# Euclidean function for finding all neighbors in range.

refFindNeighbors <- function(X, threshold, type="euclidean") {
    out.dist <- out.indx <- vector("list", nrow(X))
    tX <- t(X)
    for (j in seq_len(nrow(X))) {
        diff <- tX - X[j,]
        if (type=="euclidean") {
            targets <- sqrt(colSums(diff^2))
        } else {
            targets <- colSums(abs(diff))
        }

        chosen <- setdiff(which(targets <= threshold), j)
        targets <- targets[chosen]
        o <- order(targets)
        out.indx[[j]] <- chosen[o]
        out.dist[[j]] <- targets[o]
    }
    list(index=out.indx, distance=out.dist)
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

        chosen <- which(targets <= threshold)
        targets <- targets[chosen]
        o <- order(targets)
        out.indx[[j]] <- chosen[o]
        out.dist[[j]] <- targets[o]
    }
    list(index=out.indx, distance=out.dist)
}

#################################################
# Setting up some common function for KNN>

refFindKNN <- function(X, k, type="euclidean") {
    collected.index <- collected.dist <- list()
    tX <- t(X)

    for (i in seq_len(nrow(X))) {
        if (type == "euclidean") {
            all.dist <- sqrt(colSums((tX - X[i,])^2))
        } else {
            all.dist <- colSums(abs(tX - X[i,]))
        }

        o <- order(all.dist)
        keep <- head(setdiff(o, i), k)
        collected.index[[i]] <- keep
        collected.dist[[i]] <- all.dist[keep]
    }
    
    list(
        index=do.call(rbind, collected.index),
        distance=do.call(rbind, collected.dist)
    )
}

refQueryKNN <- function(X, Y, k, type="euclidean") {
    collected.index <- collected.dist <- list()
    tX <- t(X)

    for (i in seq_len(nrow(Y))) {
        if (type == "euclidean") {
            all.dist <- sqrt(colSums((tX - Y[i,])^2))
        } else {
            all.dist <- colSums(abs(tX - Y[i,]))
        }

        o <- order(all.dist)
        keep <- head(o, k)
        collected.index[[i]] <- keep
        collected.dist[[i]] <- all.dist[keep]
    }
    
    list(
        index=do.call(rbind, collected.index),
        distance=do.call(rbind, collected.dist)
    )
}
