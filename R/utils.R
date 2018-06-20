#' @importFrom BiocParallel bpworkers
.split_jobs <- function (jobs, BPPARAM) 
# Assigns a vector of job indices to workers.
# Returns a list of job indices, one per worker.
#
# written by Aaron Lun
# created 19 June 2018
{
    ncores <- bpworkers(BPPARAM)
    njobs <- length(jobs)

    starting <- as.integer(seq(1, njobs + 1, length.out = ncores + 1))
    jobsize <- diff(starting)
    starting <- starting[-length(starting)]

    output <- vector("list", ncores)
    for (i in seq_len(ncores)) {
        idx <- starting[i] - 1L + seq_len(jobsize)
        output[[i]] <- jobs[idx]
    }
    
    return(output)
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

    if (!is.null(subset)) { 
        dummy <- dummy[subset]
    }
    out <- unname(dummy)
    if (any(is.na(out))) {
        stop("'subset' indices out of range of 'x'")
    }
    return(out)
}

