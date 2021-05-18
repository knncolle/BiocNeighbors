#' @importFrom BiocParallel bpnworkers
.assign_jobs <- function (jobs, BPPARAM) 
# Assigns a vector of job indices to workers.
# Returns a list of job indices, one per worker.
{
    ncores <- bpnworkers(BPPARAM)
    if (ncores==1L) {
        return(list(jobs))
    }

    njobs <- length(jobs)
    starting <- as.integer(seq(1, njobs + 1, length.out = ncores + 1))
    jobsize <- diff(starting)
    starting <- starting[-length(starting)]

    output <- vector("list", ncores)
    for (i in seq_len(ncores)) {
        idx <- starting[i] - 1L + seq_len(jobsize[i])
        output[[i]] <- jobs[idx]
    }
    output
}

#' @importFrom BiocParallel bpnworkers
.split_matrix_for_workers <- function(mat, job.id, BPPARAM) {
    # Avoid unnecessary allocation where possible.
    if (bpnworkers(BPPARAM)==1L) {
        if (identical(job.id, seq_along(job.id))) {
            list(mat)
        } else {
            list(mat[,job.id,drop=FALSE])
        }
    } else {
        jobs <- .assign_jobs(job.id, BPPARAM)
        lapply(jobs, function(x) mat[,x,drop=FALSE])
    }
}

.subset_to_index <- function(subset, x, byrow=TRUE) 
# Converts an arbitary subset into an integer vector.
{
    if (byrow) {
        dummy <- seq_len(nrow(x))
        names(dummy) <- rownames(x)
    } else {
        dummy <- seq_len(ncol(x))
        names(dummy) <- colnames(x) 
    }

    out <- unname(dummy[subset])
    if (any(is.na(out))) {
        stop("'subset' indices out of range of 'x'")
    }
    return(out)
}

.order_to_index <- function(order) 
# Convenience function to convert from a (used) ordering vector to an indexing vector.
# Applying this returns an ordered sequence to its original state.    
{
    new.pos <- integer(length(order))
    new.pos[order] <- seq_along(new.pos)
    new.pos
}

.combine_matrices <- function(collected, i, reorder=NULL) 
# Combines NN-related matrix results across multiple cores.
{
    all.mat <- lapply(collected, "[[", i=i)
    out <- do.call(cbind, all.mat)
    if (!is.null(reorder)) { 
        out[,reorder] <- out
    }
    t(out)
}

.combine_lists <- function(collected, i, reorder=NULL) 
# Combines neighbor related list results across mutliple cores.
{
    all.lists <- lapply(collected, "[[", i=i)
    out <- unlist(all.lists, recursive=FALSE)
    out[reorder] <- out
    out
}

# To use in missing,missing-method definitions.
.default_param <- function(x) list(BNPARAM=KmknnParam())

.coerce_matrix_build <- function(X, transposed) {
    if (!transposed) {
        X <- t(X)
    }

    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }    

    if (!is.double(X)) {
        storage.mode(X) <- "double"
    }

    X
}

l2norm <- function(X, transposed=TRUE) {
    if (transposed) {
        l2norm <- colSums(X^2)
        if (any(l2norm==0)) {
            stop("L2 norms of zero detected for distance='Cosine'")
        }
        sweep(X, 2, l2norm, "/", check.margin=FALSE)
    } else {
        l2norm <- rowSums(X^2)
        if (any(l2norm==0)) {
            stop("L2 norms of zero detected for distance='Cosine'")
        }
        X/l2norm
    }
}
